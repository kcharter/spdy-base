-- | SPDY client implementation, without direct HTTP support.

module Network.SPDY.Client (ClientOptions(..),
                            ConnectionStyle(..),
                            defaultClientOptions,
                            Client,
                            client,
                            ping,
                            PingOptions(..),
                            defaultPingOptions,
                            PingResult(..),
                            initiateStream,
                            StreamOptions(..),
                            defaultStreamOptions,
                            updateWindow,
                            ConnectionKey(..),
                            Origin(..),
                            Scheme(..),
                            Host(..),
                            Milliseconds
                            ) where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Exception (finally)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (isNothing)
import Data.Time (getCurrentTime, diffUTCTime)
import Network (connectTo)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

import qualified Crypto.Random as CR
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSX

import Network.SPDY.Flags (allClear, packFlags, isSet)
import Network.SPDY.Frames
import Network.SPDY.Endpoint
import Network.SPDY.NetworkConnection (NetworkConnection)
import qualified Network.SPDY.NetworkConnection as NC
import Network.SPDY.Url

-- * Client API

-- | Options for clients.
data ClientOptions =
  ClientOptions { coptConnectionTimeoutSeconds :: Maybe Int
                  -- ^ The number of seconds without any activity before
                  -- closing a connection. If 'Nothing', there is no
                  -- timeout.
                , coptConnectionStyle :: ConnectionStyle
                  -- ^ The kind of network connection to establish
                  -- with remote endpoints. A single client supports
                  -- only one style of connection.
                }

-- | The kinds of low-level connections between SPDY endpoints.
data ConnectionStyle =
  CsTLS |
  -- ^ Use TLS over a socket. This is the normal form of connection in
  -- production.
  CsSocket |
  -- ^ Use an unencrypted socket. This is mostly intended for talking
  -- to development servers that have the option of serving SPDY
  -- without using TLS.
  CsCustom (ConnectionKey -> IO NetworkConnection)
  -- ^ Use the provided function for creating the network connection
  -- with a remote endpoint. This is intended to support testing.


-- | The default set of client options.
defaultClientOptions =
  ClientOptions { coptConnectionTimeoutSeconds = Nothing
                , coptConnectionStyle = CsTLS }

-- | A low-level SPDY client.
data Client =
  Client { clientOptions :: ClientOptions,
           -- ^ Options for this client.
           clientEndpoint :: Endpoint
           -- ^ The endpoint underlying this client.
         }

-- | Allocates a new client.
client :: ClientOptions -> IO Client
client opts = do
  ep <- endpoint stdClientInputFrameHandlers
  return $ Client { clientOptions = opts,
                    clientEndpoint = ep
                  }

-- | Estimates the round-trip time for a connection by measuring the
-- time to send a SPDY PING frame and receive the response from the
-- server.
ping :: PingOptions -> Client -> ConnectionKey -> IO PingResult
ping opts client cKey = do
  conn <- getConnection client cKey
  (thePingID, frame) <- pingFrame conn
  endTimeMVar <- newEmptyMVar
  installPingHandler conn thePingID (getCurrentTime >>= putMVar endTimeMVar)
  finally
    (do startTime <- getCurrentTime
        queueFrame conn ASAP frame
        queueFlush conn ASAP
        maybe (timeoutMillis startTime) return =<<
          timeout (micros $ pingOptsTimeout opts) (responseMillis startTime endTimeMVar))
    (removePingHandler conn thePingID)
  where timeoutMillis startTime =
          (PingTimeout . millisSince startTime) `fmap` getCurrentTime
        responseMillis startTime endTimeMVar =
          (PingResponse . millisSince startTime) `fmap` takeMVar endTimeMVar
        millisSince startTime endTime =
          fromIntegral $ round $ 1000 * toRational (diffUTCTime endTime startTime)
        micros millis = 1000 * fromIntegral millis

-- | Options for a PING request.
data PingOptions = PingOptions {
  pingOptsTimeout :: Milliseconds
  -- ^ The number of milliseconds to wait for a response from the
  -- remote end before giving up.
  } deriving (Show)

-- | The default set of PING options. Includes a timeout of 30 seconds.
defaultPingOptions :: PingOptions
defaultPingOptions = PingOptions { pingOptsTimeout = 30000 }

-- | The possible results of a PING request.
data PingResult =
  PingResponse Milliseconds |
  -- ^ The remote end responded in the given number of milliseconds.
  PingTimeout Milliseconds
  -- ^ We gave up waiting for the remote end after the given number of milliseconds.
  deriving (Eq, Show)

-- | Initiate a stream.
initiateStream :: Client
                  -- ^ The client on which to initiate the stream.
                  -> ConnectionKey
                  -- ^ Identifies the connection on which to initiate the stream.
                  -> [(HeaderName, HeaderValue)]
                  -- ^ The list of headers to send.
                  -> StreamOptions
                  -- ^ Other options for the stream, in particular the
                  -- producers and consumers.
                  -> IO StreamID
                  -- ^ The ID for the initiated stream.
initiateStream client cKey headers opts = do
  conn <- getConnection client cKey
  maybeData <- streamOptsDataProducer opts
  let halfClosed = isNothing maybeData
      flags = packFlags (if halfClosed then [SynStreamFlagFin] else [])
      prio = streamOptsPriority opts
      dataProducer = streamOptsDataProducer opts
      headerConsumer = streamOptsHeaderConsumer opts
      dataConsumer = streamOptsDataConsumer opts
  (sid, initFrame) <- synStreamFrame conn flags Nothing prio Nothing headers
  addStream conn sid prio dataProducer headerConsumer dataConsumer
  let sprio = StreamPriority prio
  queueFrame conn sprio initFrame
  maybe (return ()) (queueFrame conn sprio . ADataFrame . (DataFrame sid allClear)) maybeData
  queueFlush conn sprio
  return sid

-- | Sends a change in the window size for a stream to the remote
-- endpoint.
--
-- This function is intended for situations in which the data consumer
-- for the stream may report a window size change of zero bytes. In
-- such cases, it's possible for the remote endpoint to reduce the
-- window to zero but never be notified when more space becomes
-- available. If the remote endpoint respects SPDY flow control, it
-- will stop transferring data on the stream, and the stream will
-- stall.
--
-- If your data consumer always returns a positive change in window
-- size, then you shouldn't need to use this function. Otherwise, you
-- may need to use it to notify the remote endpoint when there is room
-- to accept more data.
updateWindow :: Client
                -- ^ The client.
                -> ConnectionKey
                -- ^ Identifies which connection within the client.
                -> StreamID
                -- ^ Identifies which stream within the connection.
                -> DeltaWindowSize
                -- ^ The change in window size, i.e. the number of
                -- additional bytes that can now be transmitted by the
                -- remote endpoint. May be zero, in which case this
                -- action has no effect.
                -> IO ()
updateWindow c cKey sid dws = do
  conn <- getConnection c cKey
  lookupStream conn sid >>=
    (maybe (return ()) $ \s ->
      sendWindowUpdate conn s dws)


-- * Supporting data types


-- | Obtains a connection, creating one if necessary.
getConnection :: Client -> ConnectionKey -> IO Connection
getConnection client cKey = do
  getOrCreateConnection (clientEndpoint client) cKey mkConnection
    where mkConnection =
            toNetworkConnection (coptConnectionStyle $ clientOptions client)

-- | Establishes a low-level network connection to the identified
-- remote endpoint.
toNetworkConnection :: ConnectionStyle -> ConnectionKey -> IO NetworkConnection
toNetworkConnection cs cKey =
  case cs of
    CsTLS ->
      do let protocol = C8.pack "spdy/3"
             tlsParams = TLS.defaultParams { TLS.pCiphers = TLSX.ciphersuite_all
                                           , TLS.onNPNServerSuggest =
                                             Just $ \protos -> do
                                               -- TODO: log instead of assuming stderr
                                               hPutStrLn stderr "protocols offered by server:"
                                               mapM_ (hPutStrLn stderr . C8.unpack) protos
                                               return protocol }
         rng <- CR.newGenIO :: IO CR.SystemRandom
         h <- uncurry connectTo (toConnectParams cKey)
         tlsCtx <- TLS.client tlsParams rng h
         TLS.handshake tlsCtx
         maybeProtocol <- TLS.getNegotiatedProtocol tlsCtx
         -- TODO: brackOnError to close the handle
         -- TODO: special SPDY errors for inability to negotiate the protocol
         maybe
           (ioError $ userError $ "Endpoint " ++ show cKey ++ " did not negotiate a next protocol.")
           (\p -> if p /= protocol
                  then ioError (userError $ "Endpoint " ++ show cKey ++ " insists on protocol " ++ show p)
                  else return $ NC.fromTLSCtx tlsCtx)
           maybeProtocol
    CsSocket ->
      NC.fromHandle `fmap` uncurry connectTo (toConnectParams cKey)
    CsCustom mkConnection ->
      mkConnection cKey

-- | A set of input frame handlers for a client endpoint.
stdClientInputFrameHandlers :: Connection -> FrameHandlers (IO ())
stdClientInputFrameHandlers conn =
  defaultIOFrameHandlers { handleDataFrame = forDataFrame
                         , handlePingFrame = forPingFrame
                         , handleSynReplyFrame = forSynReplyFrame
                         , handleHeadersFrame = forHeadersFrame }
    where forDataFrame d =
            let sid = streamID d
                flags = dataFlags d
                bytes = dataBytes d
            in lookupStream conn sid >>=
               maybe
               (streamError $ "DATA frame for unknown stream " ++ show sid)
               (\s -> do
                   let isLast = isSet DataFlagFin flags
                   dws <- ssDataConsumer s (Just bytes)
                   if isLast then endOfStream s else sendWindowUpdate conn s dws)
          forPingFrame _ p =
            let thePingID = pingID p
            in if isClientInitiated thePingID
               then removePingHandler conn thePingID >>= maybe (return ()) id
               else
                 -- we echo the exact same frame as the response
                 queueFrame conn ASAP (controlFrame conn $ APingFrame p) >>
                 queueFlush conn ASAP
          forSynReplyFrame _ sr =
            let flags = synReplyFlags sr
                sid = synReplyNewStreamID sr
                (HeaderBlock headers) = synReplyHeaderBlock sr
            in lookupStream conn sid >>=
               maybe
               (streamError ("SYN_REPLY for unknown stream ID " ++ show sid))
               (\s -> do
                   ssHeaderConsumer s (Just headers)
                   when (isSet SynReplyFlagFin flags) (endOfStream s))
          forHeadersFrame _ h =
            let sid = headersStreamID h
                flags = headersFlags h
                (HeaderBlock headers) = headersHeaderBlock h
            in lookupStream conn sid >>=
               maybe
               (streamError ("HEADERS for unknown stream ID " ++ show sid))
               (\s -> do
                   ssHeaderConsumer s (Just headers)
                   when (isSet HeadersFlagFin flags) (endOfStream s))
          logErr = logMessage conn
          endOfStream s = do
            ssHeaderConsumer s Nothing
            ssDataConsumer s Nothing
            -- TODO: in order to tell whether the stream is really
            -- finished, we need to know whether the stream is closed
            -- from our side or not.
            removeStream conn (ssStreamID s)
          streamError msg =
            logErr msg
            -- TODO: I think we're supposed to send a GOAWAY and clean up

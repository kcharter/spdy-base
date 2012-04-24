{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

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

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, modifyMVar, takeMVar, putMVar)
import Control.Exception (finally, throw)
import Control.Monad (when)
import Data.Attoparsec.ByteString (parse, IResult(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import qualified Data.Map as DM
import Data.Maybe (isNothing)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Tuple (swap)
import Network (HostName, PortID(..), connectTo)
import Network.Socket (HostAddress, HostAddress6, PortNumber)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import Codec.Zlib (initInflateWithDictionary, initDeflateWithDictionary)

import qualified Crypto.Random as CR
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSX

import Network.SPDY (spdyVersion3) -- TODO: this will lead to an
                                   -- import cycle if we re-export
                                   -- this module in Network.SPDY
import Network.SPDY.Error
import Network.SPDY.Flags (Flags, packFlags, allClear, isSet)
import Network.SPDY.Frames
import Network.SPDY.Compression
import Network.SPDY.Internal.PriorityChan (PriorityChan)
import qualified Network.SPDY.Internal.PriorityChan as PC
import Network.SPDY.Deserialize
import Network.SPDY.NetworkConnection (NetworkConnection)
import qualified Network.SPDY.NetworkConnection as NC
import Network.SPDY.Serialize
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
  Client { options :: ClientOptions,
           -- ^ Options for this client.
           endpoint :: Endpoint
           -- ^ The endpoint underlying this client.
         }

-- | A SPDY endpoint.
data Endpoint =
  EndPoint {
    epConnectionMapMVar :: MVar (DM.Map ConnectionKey Connection),
    -- ^ Connections by key.
    epInputFrameHandlers :: Connection -> FrameHandlers (IO ())
    -- ^ Creates input frame handlers for a connection.
    }

-- | Allocates a new client.
client :: ClientOptions -> IO Client
client opts = do
  cmapMVar <- newMVar DM.empty
  return $ Client { options = opts,
                    endpoint = EndPoint {
                      epConnectionMapMVar = cmapMVar,
                      epInputFrameHandlers = stdClientInputFrameHandlers
                      }
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

-- | Options for initiating streams.
data StreamOptions = StreamOptions {
  streamOptsPriority :: Priority,
  -- ^ The priority for the stream.
  streamOptsDataProducer :: IO (Maybe ByteString),
  -- ^ An action that retrieves the next chunk of data to send to the
  -- remote endpoint. 'Nothing' indicates no more data.
  streamOptsHeaderConsumer :: Maybe [(HeaderName, HeaderValue)] -> IO (),
  -- ^ An action that consumes headers that arrive from the remote
  -- endpoint. 'Nothing' indicates that there are no more headers.
  streamOptsDataConsumer :: Maybe ByteString -> IO DeltaWindowSize
  -- ^ An action that consumes bytes that arrive from the remote
  -- endpoint. 'Nothing' indicates that there is no more data.  The
  -- result is the number of bytes that are /consumed/ in the
  -- operation, and is used in flow control. If some or all of a given
  -- chunk of data must be buffered pending consumption, the change in
  -- window size can be smaller than the length of the byte string. If
  -- this action can return zero, you may need to use 'updateWindow'
  -- to avoid stalling the stream.
  }

-- | A default set of stream options that includes a medium priority
-- (4), a data producer that produces no data, and consumers that
-- simply discard their inputs.
defaultStreamOptions :: StreamOptions
defaultStreamOptions = StreamOptions {
  streamOptsPriority = Priority 4,
  streamOptsDataProducer = return Nothing,
  streamOptsHeaderConsumer = const $ return (),
  streamOptsDataConsumer = return . maybe 0 (fromIntegral . B.length)
  }

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
      updateWindow' conn s dws)

updateWindow' :: Connection -> Stream -> DeltaWindowSize -> IO ()
updateWindow' conn s dws =
  when (dws > 0) $ do
    let sprio = (StreamPriority $ ssPriority s)
        sid = ssStreamID s
    queueFrame conn sprio (windowUpdateFrame conn sid dws)
    queueFlush conn sprio

-- * Supporting data types

-- | Connections are uniquely identified by web origin, and by an @(IP address, port)@ pair.
data ConnectionKey =
  OriginKey Origin |
  -- ^ A key based on web origin.
  IP4PortKey HostAddress PortNumber |
  -- ^ A key based on an IP v4 host address and port.
  IP6PortKey HostAddress6 PortNumber
  -- ^ A key based on an IP v6 host address and port.
  deriving (Eq, Ord, Show)

-- | Converts a 'ConnectionKey' to a host name and port ID suitable for 'connectTo'.
toConnectParams :: ConnectionKey -> (HostName, PortID)
toConnectParams (OriginKey origin) = (toHostName (originHost origin), PortNumber (originPort origin))
toConnectParams (IP4PortKey ha pn) = (show ha, PortNumber pn)
toConnectParams (IP6PortKey ha pn) = (show ha, PortNumber pn)

-- | A number of milliseconds.
newtype Milliseconds = Milliseconds Int deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

-- | State associated with a single connection.
data Connection =
  Connection { connClient :: Client,
               -- ^ The client in which this connection is registered.
               connKeys :: IORef [ConnectionKey],
               -- ^ The list of connection keys under which this
               -- connection is registered.
               connSPDYVersion :: SPDYVersion,
               -- ^ The version of the protocol on this connection.
               connNextPingIDRef :: IORef PingID,
               -- ^ The next ping ID to use when sending a ping.
               connNextStreamIDRef :: IORef StreamID,
               -- ^ The next stream ID to use when initiating a stream.
               connLifeCycleState :: IORef ConnectionLifeCycleState,
               -- ^ The current stage of this connection's life cycle.
               connLastAcceptedStreamID :: IORef StreamID,
               -- ^ The last stream ID for which this endpoint sent a
               -- SYN_REPLY or RST_STREAM.
               connNetworkConnection :: NetworkConnection,
               -- ^ The network connection with the remote endpoint.
               connDeflate :: Deflate,
               -- ^ The zlib deflation (compression) context.
               connInflate :: Inflate,
               -- ^ The zlib inflation (decompression) context.
               connOutgoing :: PriorityChan OutgoingPriority OutgoingJob,
               -- ^ The priority channel used to hold out-going frames.
               connPingHandlers :: IORef (DM.Map PingID (IO ())),
               -- ^ Callbacks registered for PING frames expected from the server.
               connStreams :: IORef (DM.Map StreamID Stream)
             }

-- | The state of activity on a connection.
data ConnectionLifeCycleState =
  Open UTCTime |
  -- ^ The connection is active. The time is either the time the
  -- connection was established, or the last time we sent or received
  -- a frame.
  Closing |
  -- ^ The connection is being closed. A GOAWAY frame is being
  -- assembled and sent.
  Closed
  -- ^ The connection is now closed. A GOAWAY frame, if applicable, has been sent.
  deriving (Eq, Show)

-- | Update the time of last activity on a connection, provided the
-- connection is still active.
touch :: Connection -> IO ()
touch conn = do
  now <- getCurrentTime
  atomicModifyIORef (connLifeCycleState conn) $ \s ->
    (case s of
        Open _ -> Open now
        _ -> s,
     ())

-- | State associated with a particular stream.
data Stream =
  Stream { ssStreamID :: StreamID
           -- ^ The stream ID for this stream.
         , ssPriority :: Priority
           -- ^ The priority for data frames sent on this stream.
         , ssDataProducer :: IO (Maybe ByteString)
           -- ^ An IO action that fetches the next chunk of data to
           -- send to the server. A result of 'Nothing' indicates that
           -- there is no more data.
         , ssHeaderConsumer :: Maybe [(HeaderName, HeaderValue)] -> IO ()
           -- ^ An IO action that consumes the next batch of headers
           -- available from the remote endpoint. 'Nothing' signals
           -- that there are no more headers.
         , ssDataConsumer :: Maybe ByteString -> IO DeltaWindowSize
           -- ^ An IO action that consumes the next chunk of data
           -- available from the remote endpoint. 'Nothing' signals
           -- that there is no more data.
         }

installPingHandler :: Connection -> PingID -> IO () -> IO ()
installPingHandler conn pingID handler =
  atomicModifyIORef (connPingHandlers conn) $ \handlerMap ->
  (DM.insert pingID handler handlerMap, ())

removePingHandler :: Connection -> PingID -> IO (Maybe (IO ()))
removePingHandler conn pingID =
  atomicModifyIORef (connPingHandlers conn) $
  swap . DM.updateLookupWithKey (const $ const Nothing) pingID

addStream :: Connection
             -> StreamID
             -> Priority
             -> IO (Maybe ByteString)
             -> (Maybe [(HeaderName, HeaderValue)] -> IO ())
             -> (Maybe ByteString -> IO DeltaWindowSize)
             -> IO ()
addStream conn sid priority dataProducer headerConsumer dataConsumer =
  atomicModifyIORef (connStreams conn) $ \sm ->
  (DM.insert sid (Stream sid priority dataProducer headerConsumer dataConsumer) sm, ())

removeStream :: Connection -> StreamID -> IO ()
removeStream conn sid =
  atomicModifyIORef (connStreams conn) $ \sm -> (DM.delete sid sm, ())

lookupStream :: Connection -> StreamID -> IO (Maybe Stream)
lookupStream conn sid =
  DM.lookup sid `fmap` readIORef (connStreams conn)

serializeFrame :: Connection -> Frame -> IO ByteString
serializeFrame conn = frameToByteString (connDeflate conn)

-- | Obtains a connection, creating one if necessary.
getConnection :: Client -> ConnectionKey -> IO Connection
getConnection client cKey = do
  modifyMVar (epConnectionMapMVar $ endpoint client) $ \cm ->
    maybe (addConnection cm) (return . (cm,)) $ DM.lookup cKey cm
      where addConnection cm = do
              nc <- toNetworkConnection (coptConnectionStyle $ options client) cKey
              c <- setupConnection client cKey nc
              return (DM.insert cKey c cm, c)

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

-- | Creates and installs a connection for a given connection key and the network connection.
setupConnection :: Client -> ConnectionKey -> NetworkConnection -> IO Connection
setupConnection client cKey nc =
  do keysRef <- newIORef [cKey]
     now <- getCurrentTime
     lifeCycleStateRef <- newIORef (Open now)
     pingIDRef <- newIORef (PingID 1)
     nextStreamIDRef <- newIORef (StreamID 1)
     lastStreamIDRef <- newIORef (StreamID 0)
     pingHandlersRef <- newIORef DM.empty
     streamsRef <- newIORef DM.empty
     inflate <- initInflateWithDictionary defaultSPDYWindowBits compressionDictionary
     deflate <- initDeflateWithDictionary 6 compressionDictionary defaultSPDYWindowBits
     outgoing <- PC.newChan
     let conn = Connection { connClient = client,
                             connKeys = keysRef,
                             connSPDYVersion = spdyVersion3,
                             connNextPingIDRef = pingIDRef,
                             connNextStreamIDRef = nextStreamIDRef,
                             connLifeCycleState = lifeCycleStateRef,
                             connLastAcceptedStreamID = lastStreamIDRef,
                             connNetworkConnection = nc,
                             connInflate = inflate,
                             connDeflate = deflate,
                             connOutgoing = outgoing,
                             connPingHandlers = pingHandlersRef,
                             connStreams = streamsRef }
     -- TODO: record the thread IDs in an IORef in the connection,
     -- so we can forcibly terminate the reading thread should it
     -- be necessary
     forkIO (readFrames conn (epInputFrameHandlers (endpoint client) conn))
     forkIO (doOutgoingJobs conn)
     return conn

-- | Cleanly shuts down a connection. If given a 'GoAwayStatus', sends
-- the corresponding GOAWAY frame to the remote endpoint.
closeConnection :: Connection -> Maybe GoAwayStatus -> IO ()
closeConnection conn maybeGoAwayStatus = do
  oldStatus <- atomicModifyIORef (connLifeCycleState conn) $ \s ->
    case s of
      Open _  -> (Closing, s)
      _ -> (s, s)
  case oldStatus of
    Open _ -> do
      modifyMVar (epConnectionMapMVar $ endpoint $ connClient conn) removeMe
      maybe
        (return ())
        ((queueFrame conn ASAP =<<) . goAwayFrame conn)
        maybeGoAwayStatus
      queueStop conn ASAP $ do
        atomicModifyIORef (connLifeCycleState conn) (const (Closed, ()))
        NC.close (connNetworkConnection conn)
    _ -> return ()
  where removeMe m = do
          keys <- readIORef (connKeys conn)
          return (foldr DM.delete m keys, ())

-- | Creates a client-initiated PING frame for this connection.
pingFrame :: Connection -> IO (PingID, Frame)
pingFrame conn = do
  id <- nextPingID conn
  return (id, controlFrame conn $ APingFrame $ PingFrame id)

-- | Creates a GO_AWAY frame for this connection, with a given status.
goAwayFrame :: Connection -> GoAwayStatus -> IO Frame
goAwayFrame conn goAwayStatus = do
  lastStreamID <- getLastAcceptedStreamID conn
  return $ controlFrame conn $ AGoAwayFrame $ GoAwayFrame lastStreamID goAwayStatus

-- | Creates a SYN_STREAM frame, used to initiate a new stream.
synStreamFrame :: Connection
                  -> Flags SynStreamFlag
                  -> Maybe StreamID
                  -> Priority
                  -> Maybe Slot
                  -> [(HeaderName, HeaderValue)]
                  -> IO (StreamID, Frame)
synStreamFrame conn flags maybeAssocSID priority maybeSlot headers = do
  streamID <- nextStreamID conn
  return $ (streamID, controlFrame conn $ ASynStreamFrame $ SynStreamFrame
                      flags streamID maybeAssocSID
                      priority
                      (maybe noSlot id maybeSlot)
                      (HeaderBlock headers))

-- | Creates a WINDOW_UPDATE frame for this connection and a
-- particular stream.
windowUpdateFrame :: Connection -> StreamID -> DeltaWindowSize -> Frame
windowUpdateFrame conn sid =
  controlFrame conn . AWindowUpdateFrame . WindowUpdateFrame sid

-- | Creates a control frame with the correct protocol version for this connection.
controlFrame :: Connection -> ControlFrame -> Frame
controlFrame conn = AControlFrame (connSPDYVersion conn)

-- | Allocate the next ping ID for a connection. Note that ping IDs
-- are allowed to wrap, so we don't need to worry about numeric
-- overflow when incrementing.
nextPingID :: Connection -> IO PingID
nextPingID conn =
  atomicModifyIORef (connNextPingIDRef conn) incPingID
    where incPingID id@(PingID p) = (PingID (p + 2), id)

-- | Allocate the next stream ID for a connection. Stream IDs are not
-- allowed to wrap, so this function can fail with a 'OutOfStreamIDs'
-- error.
nextStreamID :: Connection -> IO StreamID
nextStreamID conn =
  atomicModifyIORef (connNextStreamIDRef conn) incStreamID
    where incStreamID id@(StreamID s) | s < maxBound = (StreamID (s + 2), id)
                                      | otherwise = throw OutOfStreamIDs

-- | Gets the last accepted stream ID recorded on a connection.
getLastAcceptedStreamID :: Connection -> IO StreamID
getLastAcceptedStreamID = readIORef . connLastAcceptedStreamID

-- | Changes the last accepted stream ID recorded on a connection.
setLastAcceptedStreamID :: Connection -> StreamID -> IO ()
setLastAcceptedStreamID conn streamID =
  atomicModifyIORef (connLastAcceptedStreamID conn) (const (streamID, ()))

-- | Read frames from the server, updating the connection state as the
-- frames are received.
readFrames :: Connection -> FrameHandlers (IO ()) -> IO ()
readFrames conn handlers = readFrames' B.empty
  where readFrames' bytes = do
          (errOrFrame, bytes') <- readAFrame bytes
          touch conn
          either handleError (\f -> handleInputFrame f >> readFrames' bytes') errOrFrame
        readAFrame bytes = readAFrame' (parse parseRawFrame bytes)
        readAFrame' (Fail bytes _ msg) = return (Left msg, bytes)
        readAFrame' (Partial continue) =
          NC.receiveData (connNetworkConnection conn) >>= (readAFrame' . continue)
        readAFrame' (Done bytes rawFrame) = do
          errOrFrame <- toFrame (connInflate conn) rawFrame
          either (\msg -> return (Left msg, bytes)) (\frame -> return (Right frame, bytes)) errOrFrame
        handleError msg = do
          logErr $ "error reading frames: " ++ msg
          readIORef (connLifeCycleState conn) >>= \s ->
            case s of
              Open _ -> logErr "closing connection with a protocol error" >>
                        closeConnection conn (Just GoAwayProtocolError)
              _ -> return ()
        handleInputFrame frame = do
          logErr $ "read frame:\n" ++ show frame
          handleFrame handlers frame
        logErr = logMessage conn

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
                   if isLast then endOfStream s else updateWindow' conn s dws)
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

-- | Sends an error message to the logger for a connection.
logMessage :: Connection -> String -> IO ()
-- TODO: the connection (or client) needs a logger; we
-- shouldn't just print to stderr
logMessage = const (hPutStrLn stderr)

-- | Priorities for jobs submitted to the outgoing channel.
data OutgoingPriority =
  ASAP |
  -- ^ Perform the job as soon as possible, ahead of normal stream priority.
  StreamPriority Priority
  -- ^ Perform the job with a given stream priority.
  deriving (Eq, Ord, Show)

-- | Jobs submitted on the outgoing queue.
data OutgoingJob =
  WriteFrame Frame |
  -- ^ Write a frame on the network connection to the remote endpoint.
  Flush |
  -- ^ Flush the network connection to the remote endpoint.
  Stop (IO ())
  -- ^ Stop processing jobs, and perform the given cleanup action.

-- | Queue a frame for writing to the remote host.
queueFrame :: Connection -> OutgoingPriority -> Frame -> IO ()
queueFrame conn prio = queueOutgoingJob conn prio . WriteFrame

-- | Queue a flush of the outgoing network connection.
queueFlush :: Connection -> OutgoingPriority -> IO ()
queueFlush conn prio = queueOutgoingJob conn prio Flush

-- | Queue a stop signal for the outgoing job thread.
queueStop :: Connection -> OutgoingPriority -> IO () -> IO ()
queueStop conn prio = queueOutgoingJob conn prio . Stop

queueOutgoingJob :: Connection -> OutgoingPriority -> OutgoingJob -> IO ()
queueOutgoingJob conn prio job =
  PC.send prio job (connOutgoing conn)

-- | Perform outgoing jobs, mostly writing frames and flushing the
-- outgoing network connection.
doOutgoingJobs :: Connection -> IO ()
doOutgoingJobs conn = go
  where go = PC.receive (connOutgoing conn) >>= \job ->
          case job of
            WriteFrame frame ->
              serializeFrame conn frame >>=
              NC.sendData (connNetworkConnection conn) >>
              touch conn >>
              checkForStreamAcks conn frame >>
              go
            Flush ->
              NC.flush (connNetworkConnection conn) >> go
            Stop action ->
              action
        checkForStreamAcks conn frame =
          case frame of
            AControlFrame _ cf ->
              case cf of
                ASynReplyFrame sr ->
                  setLastAcceptedStreamID conn (synReplyNewStreamID sr)
                ARstStreamFrame rs ->
                  setLastAcceptedStreamID conn (rstStreamTermStreamID rs)
                _ -> return ()
            ADataFrame _ ->
              return ()

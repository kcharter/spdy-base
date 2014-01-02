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
                            ConnectionKey(..),
                            Origin(..),
                            Scheme(..),
                            Host(..),
                            Milliseconds
                            ) where

import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as C8
import Network (connectTo)
import System.IO (hPutStrLn, stderr)

import qualified Crypto.Random as CR
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSX

import Network.SPDY.Flags (packFlags)
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
  ep <- endpoint $ EndpointOptions {
    epOptsFirstPingID = PingID 1,
    epOptsFirstStreamID = StreamID 1,
    epOptsIncomingRequestHandler = \_ _ -> return (HeaderBlock [], Nothing),
    epOptsInputFrameHandlers = stdClientInputFrameHandlers
    }
  return $ Client { clientOptions = opts,
                    clientEndpoint = ep
                  }

-- | Estimates the round-trip time for a connection by measuring the
-- time to send a SPDY PING frame and receive the response from the
-- server.
ping :: PingOptions -> Client -> ConnectionKey -> IO PingResult
ping opts client cKey =
  getConnection client cKey >>= pingRemote opts

-- | Initiate a stream.
initiateStream :: Client
                  -- ^ The client on which to initiate the stream.
                  -> ConnectionKey
                  -- ^ Identifies the connection on which to initiate the stream.
                  -> HeaderBlock
                  -- ^ The headers to send in the SYN_STREAM frame.
                  -> StreamOptions
                  -- ^ Other options for the stream.
                  -> IO (StreamID, Maybe (StreamContent -> IO ()), IO StreamContent)
                  -- ^ The ID for the initiated stream, an optional
                  -- request content pusher, a response content
                  -- puller. If the options indicated this was a
                  -- half-closed stream, the pusher will be 'Nothing'.
initiateStream client cKey headerBlock opts = do
  conn <- getConnection client cKey
  let halfClosed = streamOptsHalfClosed opts
      flags = packFlags (if halfClosed then [SynStreamFlagFin] else [])
      prio = streamOptsPriority opts
  (sid, initFrame) <- synStreamFrame conn flags Nothing prio Nothing headerBlock
  (maybeRequestPusher, responseProducer) <- addStream conn sid prio halfClosed
  let sprio = StreamPriority prio
  queueFrame conn sprio initFrame
  queueFlush conn sprio
  return (sid, maybeRequestPusher, responseProducer)

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
             tlsParams =
               TLS.defaultParamsClient { TLS.pCiphers = TLSX.ciphersuite_all
                                       , TLS.onNPNServerSuggest =
                                         Just $ \protos -> do
                                           -- TODO: log instead of assuming stderr
                                           hPutStrLn stderr "protocols offered by server:"
                                           mapM_ (hPutStrLn stderr . C8.unpack) protos
                                           return protocol }
         rng <- (liftM CR.cprgCreate CR.createEntropyPool :: IO CR.SystemRNG)
         h <- uncurry connectTo (toConnectParams cKey)
         tlsCtx <- TLS.contextNewOnHandle h tlsParams rng
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
stdClientInputFrameHandlers = defaultEndpointInputFrameHandlers

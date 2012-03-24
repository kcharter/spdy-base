{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

-- | SPDY client implementation, without direct HTTP support.

module Network.SPDY.Client (ClientOptions(..),
                            defaultClientOptions,
                            Client,
                            client,
                            ping,
                            ConnectionKey(..),
                            Origin(..),
                            Scheme(..),
                            Host(..),
                            Milliseconds
                            ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, modifyMVar, takeMVar, putMVar)
import Data.Attoparsec.ByteString (parse, IResult(..))
import Data.ByteString (ByteString, hPut, hGetSome)
import qualified Data.ByteString as B
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import qualified Data.Map as DM
import Data.String (IsString)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Tuple (swap)
import Network (HostName, PortID(..), connectTo)
import Network.Socket (HostAddress, HostAddress6, PortNumber)
import System.IO (Handle, hPutStrLn, stderr, hFlush)
import Codec.Zlib (initInflateWithDictionary, initDeflateWithDictionary)

import Network.SPDY (spdyVersion3) -- TODO: this will lead to an
                                   -- import cycle if we re-export
                                   -- this module in Network.SPDY
import Network.SPDY.Frames
import Network.SPDY.Compression
import Network.SPDY.Deserialize
import Network.SPDY.Serialize

-- * Client API

-- | Options for clients.
data ClientOptions =
  ClientOptions { coptConnectionTimeoutSeconds :: Maybe Int
                -- ^ The number of seconds without any activity before
                -- closing a connection. If 'Nothing', there is no
                -- timeout.
                }

-- | The default set of client options.
defaultClientOptions =
  ClientOptions { coptConnectionTimeoutSeconds = Nothing }

-- | A low-level SPDY client.
data Client =
  Client { options :: ClientOptions,
           -- ^ Options for this client.
           connectionMapMVar :: MVar (DM.Map ConnectionKey Connection)
           -- ^ Connections by key.
         }

-- | Allocates a new client.
client :: ClientOptions -> IO Client
client opts = do
  cmapMVar <- newMVar DM.empty
  return $ Client { options = opts,
                    connectionMapMVar = cmapMVar }

-- | Estimates the round-trip time for a connection by measuring the
-- time to send a SPDY PING frame and receive the response from the
-- server.
ping :: Client -> ConnectionKey -> IO Milliseconds
ping client cKey = do
  conn <- getConnection client cKey
  frame <- pingFrame conn
  startTime <- getCurrentTime
  endTimeMVar <- newEmptyMVar
  installPingHandler conn (pingID $ controlFrameDetails frame) (getCurrentTime >>= putMVar endTimeMVar)
  writeFrame conn frame
  flushConnection conn
  endTime <- takeMVar endTimeMVar
  return (fromIntegral $ round $ 1000 * toRational (diffUTCTime endTime startTime))
  where pingFrame conn = do
          pingID <- nextPingID conn
          return $ ControlFrame (connSPDYVersion conn) (Ping pingID)


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

-- | A web origin, as defined in RFC 6454.
data Origin = Origin { originScheme :: Scheme
                     , originHost :: Host
                     , originPort :: PortNumber } deriving (Eq, Ord, Show)

-- | The scheme part of a URI.
newtype Scheme = Scheme String deriving (Eq, Ord, Show, IsString)

-- | The host part of a URI.
newtype Host = Host String deriving (Eq, Ord, Show, IsString)

toHostName :: Host -> HostName
toHostName (Host host) = host

-- | A number of milliseconds.
newtype Milliseconds = Milliseconds Int deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

-- | State associated with a single connection.
data Connection =
  Connection { connSPDYVersion :: SPDYVersion,
               -- ^ The version of the protocol on this connection.
               connNextPingIDRef :: IORef PingID,
               -- ^ The next ping ID to use when sending a ping.
               connSocketHandle :: Handle,
               -- ^ The handle for the network connection.
               connDeflate :: Deflate,
               -- ^ The zlib deflation (compression) context.
               connInflate :: Inflate,
               -- ^ The zlib inflation (decompression) context.
               connPingHandlers :: IORef (DM.Map PingID (IO ()))
               -- ^ Callbacks registered for PING frames expected from the server.
             }

installPingHandler :: Connection -> PingID -> IO () -> IO ()
installPingHandler conn pingID handler =
  atomicModifyIORef (connPingHandlers conn) $ \handlerMap ->
  (DM.insert pingID handler handlerMap, ())

removePingHandler :: Connection -> PingID -> IO (Maybe (IO ()))
removePingHandler conn pingID =
  atomicModifyIORef (connPingHandlers conn) $
  swap . DM.updateLookupWithKey (const $ const Nothing) pingID

serializeFrame :: Connection -> Frame -> IO ByteString
serializeFrame conn = frameToByteString (connDeflate conn)

-- | Obtains a connection, creating one if necessary.
getConnection :: Client -> ConnectionKey -> IO Connection
getConnection client cKey = do
  modifyMVar (connectionMapMVar client) $ \cm ->
    maybe (addConnection cm) (return . (cm,)) $ DM.lookup cKey cm
      where addConnection cm = do
              c <- setupConnection cKey
              return (DM.insert cKey c cm, c)

-- | Establishes a connection for a given connection key.
setupConnection :: ConnectionKey -> IO Connection
setupConnection cKey =
  let (hn, p) = toConnectParams cKey
  in do h <- connectTo hn p
        pingIDRef <- newIORef (PingID 1)
        pingHandlersRef <- newIORef (DM.empty)
        inflate <- initInflateWithDictionary defaultSPDYWindowBits compressionDictionary
        deflate <- initDeflateWithDictionary 6 compressionDictionary defaultSPDYWindowBits
        let conn = Connection { connSPDYVersion = spdyVersion3,
                                connNextPingIDRef = pingIDRef,
                                connSocketHandle = h,
                                connInflate = inflate,
                                connDeflate = deflate,
                                connPingHandlers = pingHandlersRef }
        -- TODO: record the thread ID in an IORef in the connection,
        -- so we can forcibly terminate the reading thread should it
        -- be necessary
        forkIO (readFrames conn)
        return conn

-- | Allocate the next ping ID for a connection. Note that ping IDs
-- are allowed to wrap, so we don't need to worry about numeric
-- overflow when incrementing.
nextPingID :: Connection -> IO PingID
nextPingID conn =
  atomicModifyIORef (connNextPingIDRef conn) incPingID
    where incPingID id@(PingID p) = (PingID (p + 2), id)

-- | Write a frame to the server on the network connection.
writeFrame :: Connection -> Frame -> IO ()
writeFrame conn frame =
  serializeFrame conn frame >>= hPut (connSocketHandle conn)

flushConnection :: Connection -> IO ()
flushConnection = hFlush . connSocketHandle

-- | Read frames from the server, updating the connection state as the
-- frames are received.
readFrames :: Connection -> IO ()
readFrames conn = readFrames' B.empty
  where readFrames' bytes = do
          (errOrFrame, bytes') <- readAFrame bytes
          either handleError handleFrame errOrFrame
          readFrames' bytes'
        readAFrame bytes = readAFrame' (parse parseRawFrame bytes)
        readAFrame' (Fail bytes _ msg) = return (Left msg, bytes)
        readAFrame' (Partial continue) =
          hGetSome (connSocketHandle conn) 4096 >>= (readAFrame' . continue)
        readAFrame' (Done bytes rawFrame) = do
          errOrFrame <- toFrame (connInflate conn) rawFrame
          either (\msg -> return (Left msg, bytes)) (\frame -> return (Right frame, bytes)) errOrFrame
        -- TODO: if there is an error reading frames, there isn't
        -- really a graceful way to recover. This thread should
        -- stop, and the connection should be torn down.
        handleError = error . ("error reading frames: " ++)
        handleFrame frame = do
          logErr $ "read frame:\n" ++ show frame
          case frame of
            DataFrame _ _ _ ->
              logErr "Don't know how to handle data frames."
            ControlFrame _ details ->
              case details of
                Ping pingID ->
                  removePingHandler conn pingID >>= maybe (return ()) id
                _ ->
                  logErr "Don't know how to handle control frames other than pings"
        -- TODO: the connection (or client) needs a logger; we
        -- shouldn't just print to stderr
        logErr = hPutStrLn stderr
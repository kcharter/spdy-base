{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

-- | SPDY client implementation, without direct HTTP support.

module Network.SPDY.Client (ClientOptions(..),
                            defaultClientOptions,
                            Client,
                            client,
                            ping,
                            PingOptions(..),
                            defaultPingOptions,
                            PingResult(..),
                            ConnectionKey(..),
                            Origin(..),
                            Scheme(..),
                            Host(..),
                            Milliseconds
                            ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, modifyMVar, takeMVar, putMVar)
import Control.Exception (finally)
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
import System.Timeout (timeout)
import Codec.Zlib (initInflateWithDictionary, initDeflateWithDictionary)

import Network.SPDY (spdyVersion3) -- TODO: this will lead to an
                                   -- import cycle if we re-export
                                   -- this module in Network.SPDY
import Network.SPDY.Frames
import Network.SPDY.Compression
import Network.SPDY.Internal.PriorityChan (PriorityChan)
import qualified Network.SPDY.Internal.PriorityChan as PC
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
ping :: PingOptions -> Client -> ConnectionKey -> IO PingResult
ping opts client cKey = do
  conn <- getConnection client cKey
  frame <- pingFrame conn
  let thePingID = pingID $ controlFrameDetails frame
  endTimeMVar <- newEmptyMVar
  installPingHandler conn thePingID (getCurrentTime >>= putMVar endTimeMVar)
  finally
    (do startTime <- getCurrentTime
        queueFrame conn ASAP frame
        queueFlush conn ASAP
        maybe (timeoutMillis startTime) return =<<
          timeout (micros $ pingOptsTimeout opts) (responseMillis startTime endTimeMVar))
    (removePingHandler conn thePingID)
  where pingFrame conn = do
          pingID <- nextPingID conn
          return $ ControlFrame (connSPDYVersion conn) (Ping pingID)
        timeoutMillis startTime =
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
               connOutgoing :: PriorityChan OutgoingPriority OutgoingJob,
               -- ^ The priority channel used to hold out-going frames.
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
        outgoing <- PC.newChan
        let conn = Connection { connSPDYVersion = spdyVersion3,
                                connNextPingIDRef = pingIDRef,
                                connSocketHandle = h,
                                connInflate = inflate,
                                connDeflate = deflate,
                                connOutgoing = outgoing,
                                connPingHandlers = pingHandlersRef }
        -- TODO: record the thread IDs in an IORef in the connection,
        -- so we can forcibly terminate the reading thread should it
        -- be necessary
        forkIO (readFrames conn)
        forkIO (doOutgoingJobs conn)
        return conn

-- | Allocate the next ping ID for a connection. Note that ping IDs
-- are allowed to wrap, so we don't need to worry about numeric
-- overflow when incrementing.
nextPingID :: Connection -> IO PingID
nextPingID conn =
  atomicModifyIORef (connNextPingIDRef conn) incPingID
    where incPingID id@(PingID p) = (PingID (p + 2), id)

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
  Stop
  -- ^ Stop processing job.
  deriving (Show)

-- | Queue a frame for writing to the remote host.
queueFrame :: Connection -> OutgoingPriority -> Frame -> IO ()
queueFrame conn prio frame =
  PC.send prio (WriteFrame frame) (connOutgoing conn)

-- | Queue a flush of the outgoing network connection.
queueFlush :: Connection -> OutgoingPriority -> IO ()
queueFlush conn prio =
  PC.send prio Flush (connOutgoing conn)

-- | Perform outgoing jobs, mostly writing frames and flushing the
-- outgoing network connection.
doOutgoingJobs :: Connection -> IO ()
doOutgoingJobs conn = go
  where go = PC.receive (connOutgoing conn) >>= \job ->
          case job of
            WriteFrame frame ->
              serializeFrame conn frame >>=
              hPut (connSocketHandle conn) >>
              go
            Flush ->
              hFlush (connSocketHandle conn) >> go
            Stop ->
              return ()

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

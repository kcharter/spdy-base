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
                            initiateStream,
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
import Data.Enumerator (Enumerator, Iteratee)
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import qualified Data.Map as DM
import Data.String (IsString)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Tuple (swap)
import Network (HostName, PortID(..), connectTo)
import Network.Socket (HostAddress, HostAddress6, PortNumber)
import System.IO (Handle, hPutStrLn, stderr, hFlush, hClose)
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
initiateStream :: Maybe Priority
                  -- ^ The priority for the stream. 'Nothing'
                  -- indicates the default priority.
                  -> [(HeaderName, HeaderValue)]
                  -- ^ The list of headers to send.
                  -> Enumerator ByteString IO ()
                  -- ^ A possible empty supply of byte strings to send
                  -- to the remote endpoint.
                  -> Iteratee (HeaderName, HeaderValue) IO ()
                  -- ^ The consumer of headers that arrive from the
                  -- remote endpoint.
                  -> Iteratee ByteString IO ()
                  -- ^ The consumer of bytes that arrive from the
                  -- remote endpoint.
                  -> IO StreamID
                  -- ^ The ID for the initiated stream.
initiateStream maybePriority headers dataProducer headerConsumer dataConsumer =
  error "ni"

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
  Connection { connClient :: Client,
               -- ^ The client in which this connection is registered.
               connKeys :: IORef [ConnectionKey],
               -- ^ The list of connection keys under which this
               -- connection is registered.
               connSPDYVersion :: SPDYVersion,
               -- ^ The version of the protocol on this connection.
               connNextPingIDRef :: IORef PingID,
               -- ^ The next ping ID to use when sending a ping.
               connLifeCycleState :: IORef ConnectionLifeCycleState,
               -- ^ The current stage of this connection's life cycle.
               connLastAcceptedStreamID :: IORef StreamID,
               -- ^ The last stream ID for which this endpoint sent a
               -- SYN_REPLY or RST_STREAM.
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
              c <- setupConnection client cKey
              return (DM.insert cKey c cm, c)

-- | Establishes a connection for a given connection key.
setupConnection :: Client -> ConnectionKey -> IO Connection
setupConnection client cKey =
  let (hn, p) = toConnectParams cKey
  in do h <- connectTo hn p
        keysRef <- newIORef [cKey]
        now <- getCurrentTime
        lifeCycleStateRef <- newIORef (Open now)
        pingIDRef <- newIORef (PingID 1)
        streamIDRef <- newIORef (StreamID 0)
        pingHandlersRef <- newIORef (DM.empty)
        inflate <- initInflateWithDictionary defaultSPDYWindowBits compressionDictionary
        deflate <- initDeflateWithDictionary 6 compressionDictionary defaultSPDYWindowBits
        outgoing <- PC.newChan
        let conn = Connection { connClient = client,
                                connKeys = keysRef,
                                connSPDYVersion = spdyVersion3,
                                connNextPingIDRef = pingIDRef,
                                connLifeCycleState = lifeCycleStateRef,
                                connLastAcceptedStreamID = streamIDRef,
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
      modifyMVar (connectionMapMVar $ connClient conn) removeMe
      maybe
        (return ())
        ((queueFrame conn ASAP =<<) . goAwayFrame conn)
        maybeGoAwayStatus
      queueStop conn ASAP $ do
        atomicModifyIORef (connLifeCycleState conn) (const (Closed, ()))
        hClose (connSocketHandle conn)
    _ -> return ()
  where removeMe m = do
          keys <- readIORef (connKeys conn)
          return (foldr DM.delete m keys, ())

-- | Creates a client-initiated PING frame for this connection.
pingFrame :: Connection -> IO Frame
pingFrame conn =
  (controlFrame conn . Ping) `fmap` nextPingID conn

-- | Creates a GO_AWAY frame for this connection, with a given status.
goAwayFrame :: Connection -> GoAwayStatus -> IO Frame
goAwayFrame conn goAwayStatus = do
  lastStreamID <- getLastAcceptedStreamID conn
  return $ controlFrame conn $ GoAway lastStreamID goAwayStatus

-- | Creates a control frame with the correct protocol version for this connection.
controlFrame :: Connection -> ControlFrameDetails -> Frame
controlFrame conn = ControlFrame (connSPDYVersion conn)

-- | Allocate the next ping ID for a connection. Note that ping IDs
-- are allowed to wrap, so we don't need to worry about numeric
-- overflow when incrementing.
nextPingID :: Connection -> IO PingID
nextPingID conn =
  atomicModifyIORef (connNextPingIDRef conn) incPingID
    where incPingID id@(PingID p) = (PingID (p + 2), id)

-- | Gets the last accepted stream ID recorded on a connection.
getLastAcceptedStreamID :: Connection -> IO StreamID
getLastAcceptedStreamID = readIORef . connLastAcceptedStreamID

-- | Changes the last accepted stream ID recorded on a connection.
setLastAcceptedStreamID :: Connection -> StreamID -> IO ()
setLastAcceptedStreamID conn streamID =
  atomicModifyIORef (connLastAcceptedStreamID conn) (const (streamID, ()))

-- | Read frames from the server, updating the connection state as the
-- frames are received.
readFrames :: Connection -> IO ()
readFrames conn = readFrames' B.empty
  where readFrames' bytes = do
          (errOrFrame, bytes') <- readAFrame bytes
          touch conn
          either handleError (\f -> handleFrame f >> readFrames' bytes') errOrFrame
        readAFrame bytes = readAFrame' (parse parseRawFrame bytes)
        readAFrame' (Fail bytes _ msg) = return (Left msg, bytes)
        readAFrame' (Partial continue) =
          hGetSome (connSocketHandle conn) 4096 >>= (readAFrame' . continue)
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
        handleFrame frame = do
          logErr $ "read frame:\n" ++ show frame
          case frame of
            DataFrame _ _ _ ->
              logErr "Don't know how to handle data frames."
            ControlFrame _ details ->
              case details of
                Ping pingID | isClientInitiated pingID ->
                  removePingHandler conn pingID >>= maybe (return ()) id
                Ping _ ->
                  -- we echo the exact same frame as the response
                  queueFrame conn ASAP frame >>
                  queueFlush conn ASAP
                _ ->
                  logErr "Don't know how to handle control frames other than pings"
        -- TODO: the connection (or client) needs a logger; we
        -- shouldn't just print to stderr
        logErr = hPutStrLn stderr

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
              hPut (connSocketHandle conn) >>
              touch conn >>
              checkForStreamAcks conn frame >>
              go
            Flush ->
              hFlush (connSocketHandle conn) >> go
            Stop action ->
              action
        checkForStreamAcks conn frame =
          case frame of
            ControlFrame _ details ->
              case details of
                SynReply _ streamID _ ->
                  setLastAcceptedStreamID conn streamID
                RstStream streamID _ ->
                  setLastAcceptedStreamID conn streamID
                _ -> return ()
            DataFrame _ _ _ ->
              return ()

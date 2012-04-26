{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Network.SPDY.Endpoint
       ( -- * Creating endpoints
         Endpoint,
         EndpointOptions(..),
         endpoint,
         -- * Connections
         ConnectionKey(..),
         toConnectParams,
         Connection,
         getOrCreateConnection,
         -- * Pings and ping handlers
         pingRemote,
         PingOptions(..),
         defaultPingOptions,
         PingResult(..),
         Milliseconds,
         installPingHandler,
         removePingHandler,
         -- * Flow control
         sendWindowUpdate,
         -- * Streams
         StreamOptions(..),
         defaultStreamOptions,
         Stream(..),
         lookupStream,
         addStream,
         removeStream,
         -- * Creating frames
         pingFrame,
         synStreamFrame,
         controlFrame,
         -- * Output on a stream
         OutgoingPriority(..),
         queueFrame,
         queueFlush,
         -- * Utilities
         logMessage)
       where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, modifyMVar, putMVar, takeMVar)
import Control.Exception (throw, finally)
import Control.Monad (when)
import Data.Attoparsec.ByteString (parse, IResult(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import qualified Data.Map as DM
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Tuple (swap)
import Network (HostName, PortID(..))
import Network.Socket (HostAddress, HostAddress6, PortNumber)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import Codec.Zlib (initInflateWithDictionary, initDeflateWithDictionary)

import Network.SPDY (spdyVersion3) -- TODO: this will lead to an
                                   -- import cycle if we re-export
                                   -- this module in Network.SPDY
import Network.SPDY.Error
import Network.SPDY.Flags (Flags)
import Network.SPDY.Frames
import Network.SPDY.Compression
import Network.SPDY.Internal.PriorityChan (PriorityChan)
import qualified Network.SPDY.Internal.PriorityChan as PC
import Network.SPDY.Deserialize
import Network.SPDY.NetworkConnection (NetworkConnection)
import qualified Network.SPDY.NetworkConnection as NC
import Network.SPDY.Serialize
import Network.SPDY.Url

-- | A SPDY endpoint, i.e. a client or server. This type captures
-- functionality that is common to clients and servers.
data Endpoint =
  EndPoint {
    epConnectionMapMVar :: MVar (DM.Map ConnectionKey Connection),
    -- ^ Connections by key.
    epFirstPingID :: PingID,
    -- ^ The first ping ID on a new connection.
    epFirstStreamID :: StreamID,
    -- ^ The first stream ID on a new connection.
    epInputFrameHandlers :: Connection -> FrameHandlers (IO ())
    -- ^ Creates input frame handlers for a connection.
    }

-- | Options for creating new endpoints.
data EndpointOptions =
  EndpointOptions {
    epOptsFirstPingID :: PingID,
    -- ^ The first ping ID to issue on a connection. For a client,
    -- this must be odd, and for a server even.
    epOptsFirstStreamID :: StreamID,
    -- ^ The first stream ID to issue on a connection. For a client,
    -- this must be odd, and for a server it must be even and
    -- positive.
    epOptsInputFrameHandlers :: Connection -> FrameHandlers (IO ())
    -- ^ A function to create handlers for frames from the remote
    -- endpoint on the other end of a connection.
    }

-- | Creates a new endpoint.
endpoint :: EndpointOptions -> IO Endpoint
endpoint options = do
  cmapMVar <- newMVar DM.empty
  return $ EndPoint {
    epConnectionMapMVar = cmapMVar,
    epFirstPingID = epOptsFirstPingID options,
    epFirstStreamID = epOptsFirstStreamID options,
    epInputFrameHandlers = epOptsInputFrameHandlers options
    }

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

-- | Sends a WINDOW_UPDATE frame for a given stream on a given
-- connection.
sendWindowUpdate :: Connection -> Stream -> DeltaWindowSize -> IO ()
sendWindowUpdate conn s dws =
  when (dws > 0) $ do
    let sprio = (StreamPriority $ ssPriority s)
        sid = ssStreamID s
    queueFrame conn sprio (windowUpdateFrame conn sid dws)
    queueFlush conn sprio

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
  Connection { connEndpoint :: Endpoint,
               -- ^ The endpoint in which this connection is registered.
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

-- | Estimates the round-trip time on a connection by measuring the
-- time to send a SPDY PING frame and receive the response from the
-- remote endpoint.
pingRemote :: PingOptions -> Connection -> IO PingResult
pingRemote opts conn = do
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

-- | Retrieves a connection from an endpoint. If no connection exists,
-- creates one using a given IO action and installs it.
getOrCreateConnection :: Endpoint
                         -> ConnectionKey
                         -> (ConnectionKey -> IO NetworkConnection)
                         -> IO Connection
getOrCreateConnection ep cKey mkConnection =
  modifyMVar (epConnectionMapMVar ep) $ \cm ->
    maybe (addConnection cm) (return . (cm,)) $ DM.lookup cKey cm
      where addConnection cm = do
              nc <- mkConnection cKey
              c <- setupConnection ep cKey nc
              return (DM.insert cKey c cm, c)

-- | Creates and installs a connection for a given connection key and the network connection.
setupConnection :: Endpoint -> ConnectionKey -> NetworkConnection -> IO Connection
setupConnection ep cKey nc =
  do keysRef <- newIORef [cKey]
     now <- getCurrentTime
     lifeCycleStateRef <- newIORef (Open now)
     pingIDRef <- newIORef (epFirstPingID ep)
     nextStreamIDRef <- newIORef (epFirstStreamID ep)
     lastStreamIDRef <- newIORef (StreamID 0)
     pingHandlersRef <- newIORef DM.empty
     streamsRef <- newIORef DM.empty
     inflate <- initInflateWithDictionary defaultSPDYWindowBits compressionDictionary
     deflate <- initDeflateWithDictionary 6 compressionDictionary defaultSPDYWindowBits
     outgoing <- PC.newChan
     let conn = Connection { connEndpoint = ep,
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
     forkIO (readFrames conn (epInputFrameHandlers ep conn))
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
      modifyMVar (epConnectionMapMVar $ connEndpoint conn) removeMe
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

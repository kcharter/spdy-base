{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Network.SPDY.Endpoint
       ( -- * Creating endpoints
         RequestHandler,
         Endpoint,
         EndpointOptions(..),
         endpoint,
         -- * Connections
         ConnectionKey(..),
         toConnectParams,
         fromSockAddr,
         Connection,
         getOrCreateConnection,
         addConnection,
         -- * Pings
         pingRemote,
         PingOptions(..),
         defaultPingOptions,
         PingResult(..),
         Milliseconds,
         -- * Flow control
         sendWindowUpdate,
         -- * Streams
         StreamContent,
         moreHeaders,
         moreData,
         isLast,
         forContent,
         StreamOptions(..),
         defaultStreamOptions,
         Stream,
         lookupStream,
         addStream,
         removeStream,
         -- * Input frame handlers
         defaultEndpointInputFrameHandlers,
         -- * Creating frames
         dataFrame,
         pingFrame,
         synStreamFrame,
         headersFrame,
         windowUpdateFrame,
         rstStreamFrame,
         controlFrame,
         -- * Output on a stream
         OutgoingPriority(..),
         queueFrame,
         queueFlush,
         -- * Utilities
         logMessage)
       where

import Control.Arrow (first)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, modifyMVar, putMVar, takeMVar)
import Control.Exception (throw, finally)
import Control.Monad (when, unless, void)
import Data.Attoparsec.ByteString (parse, IResult(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import qualified Data.Map as DM
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Tuple (swap)
import Network (HostName, PortID(..))
import Network.Socket (HostAddress, HostAddress6, PortNumber, SockAddr(..))
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)
import Codec.Zlib (initInflateWithDictionary, initDeflateWithDictionary)

import Network.SPDY (spdyVersion3) -- TODO: this will lead to an
                                   -- import cycle if we re-export
                                   -- this module in Network.SPDY
import Network.SPDY.Error
import Network.SPDY.Flags (Flags, isSet, packFlags, allClear)
import Network.SPDY.Frames
import Network.SPDY.Compression
import Network.SPDY.Internal.PriorityChan (PriorityChan)
import qualified Network.SPDY.Internal.PriorityChan as PC
import Network.SPDY.Deserialize
import Network.SPDY.NetworkConnection (NetworkConnection)
import qualified Network.SPDY.NetworkConnection as NC
import Network.SPDY.Serialize
import Network.SPDY.Stream
import Network.SPDY.Url

-- | A function that creates a response from an incoming request.
type RequestHandler =
  HeaderBlock
  -- ^ The headers from the incoming request, i.e. from a SYN_STREAM frame.
  -> Maybe (IO StreamContent)
  -- ^ An optional action for fetching the next piece of incoming
  -- content for the request. Will be 'Nothing' if and only if the
  -- remote endpoint requested a half-closed stream.
  -> IO (HeaderBlock, Maybe ((StreamContent -> IO ()) -> IO ()))
  -- ^ The initial headers for the responding SYN_REPLY frame, and an
  -- optional function that takes a content pusher and produces an
  -- action that pushes the remaining content for the response. Should
  -- be nothing if and only if the response has no content other than
  -- the initial headers.

-- | A SPDY endpoint, i.e. a client or server. This type captures
-- functionality that is common to clients and servers.
data Endpoint =
  Endpoint {
    epConnectionMapMVar :: MVar (DM.Map ConnectionKey Connection),
    -- ^ Connections by key.
    epFirstPingID :: PingID,
    -- ^ The first ping ID on a new connection.
    epFirstStreamID :: StreamID,
    -- ^ The first stream ID on a new connection.
    epIncomingRequestHandler :: RequestHandler,
    -- ^ Handles incoming stream requests.
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
    epOptsIncomingRequestHandler :: RequestHandler,
    -- ^ The handler for incoming requests.
    epOptsInputFrameHandlers :: Connection -> FrameHandlers (IO ())
    -- ^ A function to create handlers for frames from the remote
    -- endpoint on the other end of a connection.
    }

-- | Creates a new endpoint.
endpoint :: EndpointOptions -> IO Endpoint
endpoint options = do
  cmapMVar <- newMVar DM.empty
  return $ Endpoint {
    epConnectionMapMVar = cmapMVar,
    epFirstPingID = epOptsFirstPingID options,
    epFirstStreamID = epOptsFirstStreamID options,
    epIncomingRequestHandler = epOptsIncomingRequestHandler options,
    epInputFrameHandlers = epOptsInputFrameHandlers options
    }

-- | Options for initiating streams.
data StreamOptions = StreamOptions {
  streamOptsPriority :: Priority,
  -- ^ The priority for the stream.
  streamOptsHalfClosed :: Bool
  -- ^ Whether the stream is half-closed. On a half-closed stream,
  -- there is no further content other than the inital set of headers.
  }

-- | A default set of stream options. The stream is half closed and of
-- medium priority (4).
defaultStreamOptions :: StreamOptions
defaultStreamOptions = StreamOptions {
  streamOptsPriority = Priority 4,
  streamOptsHalfClosed = True
  }

-- | Sends a WINDOW_UPDATE frame for a given stream on a given
-- connection.
sendWindowUpdate :: Connection -> StreamID -> OutgoingPriority -> DeltaWindowSize -> IO ()
sendWindowUpdate conn sid sprio dws =
  when (dws > 0) $ do
    queueFrame conn sprio (windowUpdateFrame conn sid dws)
    queueFlush conn sprio

-- | Connections are uniquely identified by web origin, and by an @(IP address, port)@ pair.
data ConnectionKey =
  OriginKey Origin |
  -- ^ A key based on web origin.
  IP4PortKey HostAddress PortNumber |
  -- ^ A key based on an IP v4 host address and port.
  IP6PortKey HostAddress6 PortNumber |
  -- ^ A key based on an IP v6 host address and port.
  UnixSockKey String
  -- ^ A key based on a unix socket name.
  deriving (Eq, Ord, Show)

-- | Converts a 'ConnectionKey' to a host name and port ID suitable for 'connectTo'.
toConnectParams :: ConnectionKey -> (HostName, PortID)
toConnectParams (OriginKey origin) = (toHostName (originHost origin), PortNumber (originPort origin))
toConnectParams (IP4PortKey ha pn) = (show ha, PortNumber pn)
toConnectParams (IP6PortKey ha pn) = (show ha, PortNumber pn)
-- TODO should we change the return type to (Maybe (HostName, PortID))?
toConnectParams (UnixSockKey _) = error "Sorry, can't create connection parameters from a unix socket name."

-- | Converts a 'SockAddr' to a 'ConnectionKey'.
fromSockAddr :: SockAddr -> ConnectionKey
fromSockAddr (SockAddrInet pn ha) = IP4PortKey ha pn
fromSockAddr (SockAddrInet6 pn _ ha6 _) = IP6PortKey ha6 pn
fromSockAddr (SockAddrUnix name) = UnixSockKey name

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
               connIncomingBufferSize :: IORef Int,
               -- ^ The starting size for the incoming flow control data window on a new stream.
               connInitialOutgoingWindowSize :: IORef Int,
               -- ^ The starting size for the outgoing flow control data window on a new stream.
               connStreams :: IORef (DM.Map StreamID Stream)
               -- ^ The collection of active streams on the connection.
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

defaultInitialWindowSize :: Int
defaultInitialWindowSize = 64 * 1024

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

-- | Indicates whether a ping ID could have been initiated by this
-- endpoint. That is, whether it has the same parity as the first ping
-- ID for this endpoint.
isInitiatedHere :: Endpoint -> PingID -> Bool
isInitiatedHere ep pid =
  isClientInitiated firstPid && isClientInitiated pid ||
  isServerInitiated firstPid && isServerInitiated pid
  where firstPid = epFirstPingID ep

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
             -> Bool
             -> IO (Maybe (StreamContent -> IO ()), IO StreamContent)
addStream conn sid priority halfClosed = do
  odws <- getInitialOutgoingWindowSize conn
  ibs  <- getIncomingBufferSize conn
  let nso = NewStreamOpts { nsoInitialOutgoingWindowSize = odws
                          , nsoIncomingBufferSize = ibs
                          , nsoHalfClosed = halfClosed
                          , nsoOutgoingHandler = outgoingHandler
                          , nsoWindowUpdateHandler = queueWindowUpdate
                          , nsoResetHandler = queueReset }
  (s, maybePusher, puller) <- newStream sid nso
  atomicModifyIORef (connStreams conn) $ \sm ->
    (DM.insert sid s sm, ())
  return (maybePusher, puller)
  where outgoingHandler = forContent queueHeaders queueData
        queueHeaders headerBlock last =
          let flags = packFlags (if last then [HeadersFlagFin] else [])
          in queueFrame conn sprio (headersFrame conn flags sid headerBlock)
        queueData bytes last =
          let flags = packFlags (if last then [DataFlagFin] else [])
          in queueFrame conn sprio (dataFrame sid flags bytes)
        queueWindowUpdate = sendWindowUpdate conn sid sprio
        queueReset = queueFrame conn sprio . (rstStreamFrame conn sid)
        sprio = StreamPriority priority

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
    maybe (createAndAddConnection cm) (return . (cm,)) $ DM.lookup cKey cm
      where createAndAddConnection cm = do
              nc <- mkConnection cKey
              c <- setupConnection ep cKey nc
              return (DM.insert cKey c cm, c)

-- | Adds a connection under a given connection key.
addConnection :: Endpoint -> ConnectionKey -> NetworkConnection -> IO ()
addConnection ep cKey nc =
  modifyMVar (epConnectionMapMVar ep) $ \cm -> do
    c <- setupConnection ep cKey nc
    return (DM.insert cKey c cm, ())

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
     outgoingWindowSizeRef <- newIORef defaultInitialWindowSize
     incomingBufSizeRef <- newIORef defaultInitialWindowSize
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
                             connInitialOutgoingWindowSize = outgoingWindowSizeRef,
                             connIncomingBufferSize = incomingBufSizeRef,
                             connStreams = streamsRef }
     -- TODO: record the thread IDs in an IORef in the connection,
     -- so we can forcibly terminate the reading thread should it
     -- be necessary
     forkIO (readFrames conn (epInputFrameHandlers ep conn))
     forkIO (doOutgoingJobs conn)
     incomingWindowSize <- getIncomingBufferSize conn
     queueFrame conn ASAP (unpersistedSettingsFrame conn
                           [(SettingsInitialWindowSize,
                             fromIntegral incomingWindowSize)])
     queueFlush conn ASAP
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
pingFrame :: Connection
             -- ^ The connection from which to obtain the protocol
             -- version and the ping ID.
             -> IO (PingID, Frame)
             -- ^ The resulting ping ID and frame. The ping ID is
             -- carried in the frame, but it is included as a
             -- convenience.
pingFrame conn = do
  id <- nextPingID conn
  return (id, controlFrame conn $ APingFrame $ PingFrame id)

-- | Creates a GO_AWAY frame for this connection, with a given status.
goAwayFrame :: Connection
               -- ^ The connection from which to obtain the protocol version.
               -> GoAwayStatus
               -- ^ The reason for tearing down the connection.
               -> IO Frame
               -- ^ The resulting frame.
goAwayFrame conn goAwayStatus = do
  lastStreamID <- getLastAcceptedStreamID conn
  return $ controlFrame conn $ AGoAwayFrame $ GoAwayFrame lastStreamID goAwayStatus

-- | Creates a SYN_STREAM frame, used to initiate a new stream.
synStreamFrame :: Connection
                  -- ^ The connnection from which to obtain the
                  -- protocol version and the new stream ID.
                  -> Flags SynStreamFlag
                  -- ^ Flags for the frame.
                  -> Maybe StreamID
                  -- ^ An optional associated stream ID, used in server push streams.
                  -> Priority
                  -- ^ The priority of the new stream.
                  -> Maybe Slot
                  -- ^ An optional credential slot to use for
                  -- authenticating at the remote end.
                  -> HeaderBlock
                  -- ^ The headers to include in the frame.
                  -> IO (StreamID, Frame)
                  -- ^ The resulting stream ID and frame. While the ID
                  -- is also in the frame, it's included in the result
                  -- as a convenience.
synStreamFrame conn flags maybeAssocSID priority maybeSlot headerBlock = do
  streamID <- nextStreamID conn
  return $ (streamID, controlFrame conn $ ASynStreamFrame $ SynStreamFrame
                      flags streamID maybeAssocSID
                      priority
                      (maybe noSlot id maybeSlot)
                      headerBlock)

-- | Creates a SYN_REPLY frame for a new stream in the enclosing connection.
synReplyFrame :: Connection -> Flags SynReplyFlag -> StreamID -> HeaderBlock -> Frame
synReplyFrame conn flags sid =
  controlFrame conn . ASynReplyFrame . SynReplyFrame flags sid

-- | Creates a DATA frame for a particular stream.
dataFrame :: StreamID
             -- ^ Identifies the stream to which the data belongs.
             -> Flags DataFlag
             -- ^ Flags for the frame.
             -> ByteString
             -- ^ The data itself.
             -> Frame
             -- ^ The resulting frame.
dataFrame sid flags = ADataFrame . DataFrame sid flags

-- | Creates a HEADERS frame for this connection and a particular stream.
headersFrame :: Connection
                -- ^ The connection from which to obtain the protocol version.
                -> Flags HeadersFlag
                -- ^ Flags for the frame.
                -> StreamID
                -- ^ Identifies the stream to which the headers belong.
                -> HeaderBlock
                -- ^ The headers to include in the frame.
                -> Frame
                -- ^ The resulting frame.
headersFrame conn flags sid =
  controlFrame conn . AHeadersFrame . HeadersFrame flags sid

-- | Creates a WINDOW_UPDATE frame for this connection and a
-- particular stream.
windowUpdateFrame :: Connection
                     -- ^ The connection from which to obtain the protocol version.
                     -> StreamID
                     -- ^ Identifies the stream to which the update pertains.
                     -> DeltaWindowSize
                     -- ^ The change in data window size for this endpoint.
                     -> Frame
                     -- ^ The resulting frame.
windowUpdateFrame conn sid =
  controlFrame conn . AWindowUpdateFrame . WindowUpdateFrame sid

-- | Creates a RST_STREAM frame for a connection and stream.
rstStreamFrame :: Connection
                  -- ^ The connection from which to obtain the protocol version.
                  -> StreamID
                  -- ^ Identifies the stream being terminated.
                  -> TerminationStatus
                  -- ^ Identifies the reason for the termination.
                  -> Frame
                  -- ^ The resulting frame.
rstStreamFrame conn sid =
  controlFrame conn . ARstStreamFrame . RstStreamFrame sid

-- | Creates a SETTINGS frame that makes no use of any of the persistence flags.
unpersistedSettingsFrame :: Connection -> [(SettingID, SettingValue)] -> Frame
unpersistedSettingsFrame conn pairs =
  settingsFrame conn allClear (map (first addClearFlags) pairs)
  where addClearFlags sid = SettingIDAndFlags { settingIDFlags = allClear
                                              , settingID = sid }

-- | Creates a SETTINGS frame for a connection.
settingsFrame :: Connection -> Flags SettingsFlag -> [(SettingIDAndFlags, SettingValue)] -> Frame
settingsFrame conn flags =
  controlFrame conn . ASettingsFrame . SettingsFrame flags

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

-- | Gets the incoming buffer size for new streams.
getIncomingBufferSize :: Connection -> IO Int
getIncomingBufferSize = readIORef . connIncomingBufferSize

-- | Sets the incoming buffer size for new streams.
setIncomingBufferSize :: Connection -> Int -> IO ()
setIncomingBufferSize conn size =
  atomicModifyIORef (connIncomingBufferSize conn) (const (size, ()))

-- | Gets the initial outgoing data window size for new streams.
getInitialOutgoingWindowSize :: Connection -> IO Int
getInitialOutgoingWindowSize = readIORef . connInitialOutgoingWindowSize

-- | Sets the initial outgoing data window size for new streams.
setInitialOutgoingWindowSize :: Connection -> Int -> IO ()
setInitialOutgoingWindowSize conn size =
  atomicModifyIORef (connInitialOutgoingWindowSize conn) (const (size, ()))

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

-- | A set of default input frame handlers for an endpoint. Handles
-- PING frames in particular.
defaultEndpointInputFrameHandlers :: Connection -> FrameHandlers (IO ())
defaultEndpointInputFrameHandlers conn =
  defaultIOFrameHandlers {
    handlePingFrame = \_ p ->
     let thePingID = pingID p
     in if isInitiatedHere (connEndpoint conn) thePingID
        then removePingHandler conn thePingID >>= maybe (return ()) id
        else
          -- we echo the exact same frame as the response
          queueFrame conn ASAP (controlFrame conn $ APingFrame p) >>
          queueFlush conn ASAP,
    handleDataFrame = forDataFrame,
    handleSynStreamFrame = forSynStreamFrame,
    handleSynReplyFrame = forSynReplyFrame,
    handleSettingsFrame = forSettingsFrame,
    handleHeadersFrame = forHeadersFrame,
    handleWindowUpdateFrame = forWindowUpdateFrame
    }
  where forDataFrame d =
          forStream d $ \s ->
          do let flags = dataFlags d
                 bytes = dataBytes d
             queued <- addIncomingData s bytes (isSet DataFlagFin flags)
             -- TODO: this should result in a RST_STREAM with a
             -- status of 'flow control error', and then tearing
             -- down the stream
             unless queued (error "don't know how to handle this")
        forSynStreamFrame _ ss =
          let sid = streamOf ss
              flags = synStreamFlags ss
              halfClosed = isSet SynStreamFlagFin flags
              handler = epIncomingRequestHandler $ connEndpoint conn
              prio = synStreamPriority ss
              sprio = StreamPriority prio
              headers = synStreamHeaderBlock ss
              startNewStream = do
                -- TODO: should addStream take another argument saying
                -- whether the remote side is half-closed?
                (Just pusher, puller) <- addStream conn sid prio False
                void $ forkIO $ do
                  -- TODO: what about the unidirectional flag?
                  -- TODO: catch exceptions when pushing content
                  (respHeaders, maybeRespRest) <- handler headers (if halfClosed then Nothing else Just puller)
                  let respFlags = packFlags (maybe [SynReplyFlagFin] (const []) maybeRespRest)
                  queueFrame conn sprio (synReplyFrame conn respFlags sid respHeaders)
                  maybe (return ()) ($ pusher) maybeRespRest
                  queueFlush conn sprio
                  removeStream conn sid
          in lookupStream conn sid >>=
             maybe
             startNewStream
             (\_ -> queueFrame conn ASAP (rstStreamFrame conn sid ProtocolError))
        forSynReplyFrame _ sr =
          forStream sr $ \s ->
          do let flags = synReplyFlags sr
                 headerBlock = synReplyHeaderBlock sr
             addIncomingHeaders s headerBlock (isSet SynReplyFlagFin flags)
        forSettingsFrame _ st =
          maybe
          (return ())
          (setInitialOutgoingWindowSize conn . fromIntegral)
          (extractSetting SettingsInitialWindowSize $ settingsPairs st)
        forHeadersFrame _ h =
          forStream h $ \s ->
          do let flags = headersFlags h
                 headerBlock = headersHeaderBlock h
             addIncomingHeaders s headerBlock (isSet HeadersFlagFin flags)
        forWindowUpdateFrame _ w =
          forStream w $ \s -> updateOutgoingWindowSize s (windowUpdateDeltaWindowSize w)
        forStream :: (WithFrameType f, WithStream f) => f -> (Stream -> IO ()) -> IO ()
        forStream frame action =
          let sid = streamOf frame
          in lookupStream conn sid >>=
             maybe
             (streamError frame (frameTypeName frame ++ " for unknown stream ID " ++ show sid))
          action
        -- TODO: a stream error may require a RST_STREAM or a GOAWAY
        -- frame and then some cleanup
        streamError :: (WithFrameType f, WithStream f) => f -> String -> IO ()
        streamError frame msg = do
          logErr msg
          unless (frameTypeOf frame == RstStream) $
          -- TODO: make termination status a parameter, close stream
            queueFrame conn ASAP (rstStreamFrame conn (streamOf frame) InvalidStream)
        logErr = logMessage conn

extractSetting :: SettingID -> [(SettingIDAndFlags, SettingValue)] -> Maybe SettingValue
extractSetting sid pairs = lookup sid (map (first settingID) pairs)

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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |

Data types for representing SPDY 3 frames.

The definitions here are derived from the SPDY 3 draft specification
accessible from

<http://dev.chromium.org/spdy/spdy-protocol>

Many of the descriptive comments in this module are taken almost
word-for-word from the draft spec.

-}


module Network.SPDY.Frames where

import Control.Monad.Trans (MonadIO)
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8

import Network.SPDY.Flags

-- | A lightly-processed SPDY frame. This is intended as an
-- intermediate form between raw bytes and a 'Frame'.
data RawFrame =
  RawFrame
  { frameHeader :: RawFrameHeader
  , flagsByte :: Word8
  , payload :: ByteString
  } deriving (Eq, Show, Read)

-- | Get the number of bytes in the payload of a raw frame.
payloadLength :: RawFrame -> DataLength
payloadLength = fromIntegral . BSC8.length . payload

-- | The parts of a raw frame that are unique to control frames and
-- data frames. This represents the contents of the first four bytes
-- of a SPDY frame.
data RawFrameHeader =
  -- | A control frame header.
  ControlFrameHeader
  { controlSpdyVersion :: SPDYVersion
    -- ^ The version of the SPDY protocol.
  , controlType :: Word16
    -- ^ The type of control frame. This determines how the contents
    -- of the payload are interpreted.
  } |
  -- | A data frame header.
  DataFrameHeader
  { dataStreamID :: StreamID
    -- ^ The ID for the stream to which the data belongs.
  }
  deriving (Eq, Show, Read)

-- | The more heavily-processed form of a SPDY frame. For control
-- frames, the data payload is replaced by a data type representing
-- the contents of the payload.
data Frame =
  -- | A control frame. Control frames are used for setting up and
  -- tearing down streams, and for exchanging information relevant to
  -- the SPDY protocol itself.
  AControlFrame SPDYVersion ControlFrame |
  -- | A data frame. Data frames carry raw data between a sender and a
  -- receiver.
  ADataFrame DataFrame
  deriving (Eq, Show, Read)

-- | The contents of a DATA frame.
data DataFrame =
  DataFrame
  { streamID :: StreamID
    -- ^ Identifies the stream to which the accompanying data belongs.
  , dataFlags :: Flags DataFlag
    -- ^ Flags for the data frame.
  , dataBytes :: ByteString
    -- ^ The raw data.
  }
  deriving (Eq, Show, Read)

-- | Higher-level representation of the contents of the different
-- kinds of control frames.
data ControlFrame =
  -- | Request to create a new stream.
  ASynStreamFrame SynStreamFrame |
  -- | Acknowledges receipt of a 'SynStream' frame.
  ASynReplyFrame SynReplyFrame |
  -- | Request to abnormally terminate a stream.
  ARstStreamFrame RstStreamFrame |
  -- | Exchange settings, and request or acknowledge that they have
  -- been persisted or cleared.
  ASettingsFrame SettingsFrame |
  -- | Used for estimating the minimum round-trip time from the
  -- sender. Recipients of a 'Ping' frame should send an identical
  -- frame to the sender as soon as possible. If there is other data
  -- waiting to be sent, the 'Ping' frame should take the highest
  -- priority. Each ping sent by sender should have a unique ID.
  APingFrame PingFrame |
  -- | Tells the remote endpoint to stop using this session. Once
  -- sent, the sender will not initiate any new streams on this
  -- session, and once received the receiver should not send any new
  -- requests on this session. This is intended to allow the orderly
  -- tear-down of a session.
  AGoAwayFrame GoAwayFrame |
  -- | Augments an existing stream with additional headers.
  AHeadersFrame HeadersFrame |
  -- | Informs the recipient that there is a (positive) change in the
  -- amount of free space in the sender's data transfer window. This
  -- frame is part of SPDY's approach to flow control. An endpoint's
  -- transfer window is the total capacity that the endpoint has for
  -- buffering raw stream data it receives.
  AWindowUpdateFrame WindowUpdateFrame |
  -- | Asks the server to update a slot in its credential vector for
  -- this connection. The server will overwrite any existing
  -- credential at that slot.
  ACredentialFrame CredentialFrame
  deriving (Eq, Show, Read)

data DataFlag =
  DataFlagFin |
  -- ^ The enclosing frame is the last one transmitted by the sender
  -- in this stream.
  DataFlagCompress
  -- ^ The data in the enclosing frame has been compressed.
  deriving (Eq, Show, Read)

instance Flag DataFlag where
  bit DataFlagFin = 0
  bit DataFlagCompress = 1

data SynStreamFrame =
  SynStreamFrame
  { synStreamFlags :: Flags SynStreamFlag
    -- ^ Flags for stream creation.
  , synStreamNewStreamID :: StreamID
    -- ^ Uniquely identifies the new stream.
  , synStreamAssociatedTo :: Maybe StreamID
    -- ^ Identifies a stream with which this stream is associated. If
    -- this stream is independent of all others, this must be
    -- 'Nothing'.
  , synStreamPriority :: Priority
    -- ^ The priority of the new stream.
  , synStreamSlot :: Slot
    -- ^ The index in the server's credential vector of the client
    -- certificate to be used for this request. A value of zero means
    -- that no client certificate should be associated with this
    -- stream.
  , synStreamHeaderBlock :: HeaderBlock
    -- ^ A set of headers for the stream.
  }
  deriving (Eq, Read, Show)

-- | Flags used in the 'SynStream' frame.
data SynStreamFlag =
  SynStreamFlagFin |
  -- ^ The enclosing frame is the last to be transmitted on this
  -- stream and the sender is in the half-closed state.
  SynStreamFlagUnidirectional
  -- ^ The recipient should start in the half-closed state.
  deriving (Eq, Show, Read)

instance Flag SynStreamFlag where
  bit SynStreamFlagFin = 0
  bit SynStreamFlagUnidirectional = 1

data SynReplyFrame =
  SynReplyFrame
  { synReplyFlags :: Flags SynReplyFlag
    -- ^ Flags for the reply.
  , synReplyNewStreamID :: StreamID
    -- ^ The same stream ID as in the 'SynStream' frame.
  , synReplyHeaderBlock :: HeaderBlock
    -- ^ A set of headers for the stream (unsure whether this is
    -- supposed to be the exact same set of headers as in the
    -- 'SynStream' frame).
  }
  deriving (Eq, Read, Show)

-- | Flags used in the 'SynReply' frame.
data SynReplyFlag =
  SynReplyFlagFin
  -- ^ The enclosing frame is the last to be transmitted on this
  -- stream and the sender is in the half-closed state.
  deriving (Eq, Show, Read)

instance Flag SynReplyFlag where
  bit SynReplyFlagFin = 0

data RstStreamFrame =
  RstStreamFrame
  { rstStreamTermStreamID :: StreamID
    -- ^ Identifies the stream to terminate.
  , rstStreamTermStatus :: TerminationStatus
    -- ^ Indicates the reason why the stream is being terminated.
  }
  deriving (Eq, Read, Show)

-- | The various reasons why a stream could be terminated abnormally
-- with a 'RstStream' frame.
data TerminationStatus =
  ProtocolError |
  -- ^ A generic error, used only when no more specific error applies.
  InvalidStream |
  -- ^ Returned when a frame is received for a stream which is not active.
  RefusedStream |
  -- ^ Indicates that the stream was refused before any processing has
  -- been done on the stream.
  UnsupportedVersion |
  -- ^ Indicates that the recipient of a stream does not support the
  -- SPDY version requested.
  Cancel |
  -- ^ Used by the creator of a stream to indicate that the stream is
  -- no longer needed.
  InternalError |
  -- ^ A generic error that can be used when the implementation has
  -- internally failed, not due to anything in the protocol.
  FlowControlError |
  -- ^ The endpoint detected that its peer violated the flow control
  -- protocol.
  StreamInUse |
  -- ^ The endpoint received a @SYN_REPLY@ for a stream already open.
  StreamAlreadyClosed |
  -- ^ The endpoint received a data or @SYN_REPLY@ frame for a stream
  -- which is half-closed.
  InvalidCredentials |
  -- ^ The server received a request for a resource whose origin does
  -- not have valid credentials in the client certificate vector.
  FrameTooLarge |
  -- ^ The endpoint received a frame which this implementation could
  -- not support. If FRAME_TOO_LARGE is sent for a @SYN_STREAM@,
  -- @HEADERS@, or @SYN_REPLY@ frame without fully processing the
  -- compressed portion of those frames, then the compression state
  -- will be out-of-sync with the other endpoint. In this case,
  -- senders of @FRAME_TOO_LARGE@ must close the session.
  TerminationStatusUnknown Word32
  -- ^ Some other termination status, unrecognized by this
  -- library. This is *not* part of the SPDY spec.
  deriving (Eq, Show, Read)

data SettingsFrame =
  SettingsFrame
  { settingsFlags :: Flags SettingsFlag
  , settingsPairs :: [(SettingIDAndFlags, SettingValue)]
  }
  deriving (Eq, Read, Show)

data PingFrame =
  PingFrame
  { pingID :: PingID
    -- ^ A unique ID for this ping.
  }
  deriving (Eq, Read, Show)

data GoAwayFrame =
  GoAwayFrame
  { goAwayLastGoodStreamID :: StreamID
    -- ^ The last stream ID which was accepted by the sender of this
    -- message. If no streams were accepted, must be the zero stream
    -- ID.
  , goAwayStatus :: GoAwayStatus
    -- ^ The reason for closing the session.
  }
  deriving (Eq, Read, Show)

data HeadersFrame =
  HeadersFrame
  { headersFlags :: Flags HeadersFlag
  , headersStreamID :: StreamID
    -- ^ The stream ID of the stream to which the headers apply.
  , headersHeaderBlock :: HeaderBlock
    -- ^ The set of header-value pairs.
  }
  deriving (Eq, Read, Show)

data WindowUpdateFrame =
  WindowUpdateFrame
  { windowUpdateStreamID :: StreamID
    -- ^ The ID of the stream to which this frame applies.
  , windowUpdateDeltaWindowSize :: DeltaWindowSize
    -- ^ The additional number of bytes that the sender can transmit
    -- in addition to the remaining window size.
  }
  deriving (Eq, Read, Show)

data CredentialFrame =
  CredentialFrame
  { credentialSlot :: Slot16
    -- ^ The slot in the server's credential vector in which to place the certificate.
  , credentialProof :: Proof
    -- ^ Cryptographic proof that the client has the necessary private key.
  , credentialCertificates :: [Certificate]
    -- ^ The certificate chain, starting with the leaf certificate.
  }
  deriving (Eq, Read, Show)

-- | The version of the SPDY protocol. Only 15 bits are used.
newtype SPDYVersion = SPDYVersion Word16 deriving (Eq, Show, Read)

-- | Extracts the raw version word from a 'SPDYVersion'.
rawSPDYVersion :: SPDYVersion -> Word16
rawSPDYVersion (SPDYVersion w) = w

-- | The length of the data payload in a frame. Only 24 bits are used.
newtype DataLength =
  DataLength Word32
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Num, Real, Integral)

-- | Identifies a stream within a SPDY connection. Only 31 bits are
-- used.
newtype StreamID = StreamID Word32 deriving (Eq, Ord, Show, Read)

-- | Extracts the raw stream ID word from a 'StreamID'.
rawStreamID :: StreamID -> Word32
rawStreamID (StreamID w) = w

-- | The priority of a stream. Legal numeric values range from @0@ to
-- @7@, @0@ being the highest priority.
newtype Priority = Priority Word8 deriving (Eq, Show, Read)

instance Ord Priority where
  compare (Priority p1) (Priority p2) =  compare p2 p1

-- | A position in a server's credential vector. Zero means no
-- position. This is used only in a @SYN_STREAM@ frame.
newtype Slot = Slot Word8 deriving (Eq, Show, Read)

-- | The zero slot, for convenience.
noSlot :: Slot
noSlot = Slot 0

-- | Yet another position in a server's credential vector, but for use
-- in a @CREDENTIAL@ frame. Yes, in the draft of SPDY 3, the slot
-- fields really are different sizes in the two frames. In effect,
-- this means that there are at most 255 credentials in the server's
-- credential vector. However, I've declared both types just to be
-- faithful to the spec.
newtype Slot16 = Slot16 Word16 deriving (Eq, Show, Read)

-- | A list of header names and their corresponding values.
newtype HeaderBlock =
  HeaderBlock
  { headerPairs :: [(HeaderName, HeaderValue)]
    -- ^ The headers and their values.
  }
  deriving (Eq, Show, Read)

-- | The number of headers in a block.
headerCount :: HeaderBlock -> HeaderCount
headerCount = fromIntegral . length . headerPairs

-- | The number of headers (name-value pairs) in a header block.
newtype HeaderCount =
  HeaderCount Word32 deriving (Eq, Show, Read, Enum, Ord, Num, Real, Integral)

-- | A header name.
newtype HeaderName = HeaderName ByteString deriving (Eq, Show, Read)

-- | A header value.
newtype HeaderValue = HeaderValue ByteString deriving (Eq, Show, Read)

-- | A settings ID paired with a list of settings flags.
data SettingIDAndFlags =
  SettingIDAndFlags
  { settingIDFlags :: Flags SettingIDFlag
    -- ^ The flags.
  , settingID :: SettingID
    -- ^ The setting ID itself.
  }
  deriving (Eq, Show, Read)

-- | Flags for a 'Settings' control frame.
data SettingsFlag =
  SettingsFlagClearSettings
  -- ^ When set, the client should clear and previously persisted
  -- settings. If this frame contains ID/Value pairs with the flag
  -- 'SettingIDFlagPersistValue' set, then the client will first clear
  -- its existing persisted settings and then persist the values with
  -- the flag set contained in this frame. Because persistence is
  -- implemented only on the client, this flag should be sent only by
  -- the server.
  deriving (Eq, Show, Read)

instance Flag SettingsFlag where
  bit SettingsFlagClearSettings = 0

-- | Flags that appear as part of a 'SettingID'.
data SettingIDFlag =
  SettingIDFlagPersistValue |
  -- ^ The sender of the enclosing 'Settings' frame is requesting that
  -- the recipient persist the ID/Value and return it in future
  -- 'Settings' frames sent from the sender to this recipient. Because
  -- persistence is implemented only on the client, this flag is sent
  -- only by the server.
  SettingIDFlagPersisted
  -- ^ The sender is notifiying the recipient that this ID/Value pair
  -- was previously sent to the sender by the recipient with
  -- 'SettingsIDFlagPersistValue', and the sender is returning
  -- it. Because persistence is implemented only on the client, this
  -- flag is sent only by the client.
  deriving (Eq, Show, Read)

instance Flag SettingIDFlag where
  bit SettingIDFlagPersistValue = 0
  bit SettingIDFlagPersisted = 1

-- | A setting ID. IDs defined in the specification are given their
-- own variants, with 'SettingsOther' as a catch-all for unrecognized
-- settings.
data SettingID =
  SettingsUploadBandwidth |
  -- ^ The sender's expected upload bandwidth on this channel. This
  -- number is an estimate. The value should be the integral number of
  -- kilobytes per second that the sender predicts as an expected
  -- maximum upload channel capacity.
  SettingsDownloadBandwidth |
  -- ^ The sender's expected download capacity on this channel. This
  -- number is an estimate. The value should be the integral number of
  -- kilobytes per second that the sender predicts as an expected
  -- maximum donwload channel capacity.
  SettingsRoundTripTime |
  -- ^ The sender's expected round-trip-time on this channel. The
  -- round-trip-time is defined as the minimum amount of time to send
  -- a control frame from the sender to the recipient and receive a
  -- response. The value is represented in milliseconds.
  SettingsMaxConcurrentStreams |
  -- ^ The maximum number of concurrent streams that the sender will
  -- allow. By default there is no limit. For implementors it is
  -- recommended that this value be no smaller than 100.
  SettingsCurrentCWND |
  -- ^ The sender's current TCP CWND value.
  SettingsDownloadRetransRate |
  -- ^ The sender's retransmission rate (bytes retransmitted / total
  -- bytes transmitted).
  SettingsInitialWindowSize |
  -- ^ The sender's initial window size, in bytes, for new streams.
  SettingsClientCertificateVectorSize |
  -- ^ The server's new credential vector size.
  SettingsOther Word32
  -- ^ A catch-all for other settings values that this code doesn't
  -- recognize. This is *not* part of the SPDY specification.
  deriving (Eq, Show, Read)

-- | The value for a setting.
newtype SettingValue = SettingValue Word32
                     deriving (Eq, Show, Read, Ord, Num, Enum, Real, Integral)

-- | When a client initiates a ping, it must use an odd-numbered
-- ID. When the server initiates a ping, it must use an even-numbered
-- ID. This scheme avoids accidental looping on pings, where each side
-- initiates an identical ping at the same time.
newtype PingID = PingID Word32 deriving (Eq, Ord, Show, Read)

-- | Determines whether a ping ID is for a server-initiated ping.
isServerInitiated :: PingID -> Bool
isServerInitiated (PingID id) = even id

-- | Determines whether a ping ID is for a client-initiated ping.
isClientInitiated :: PingID -> Bool
isClientInitiated (PingID id) = odd id

-- | The reasons for receiving a 'GoAway' frame.
data GoAwayStatus =
  GoAwayOK |
  -- ^ This is a normal session tear-down.
  GoAwayProtocolError |
  -- ^ A generic error, used only if no more specific error applies.
  GoAwayInternalError |
  -- ^ A generic error which can be used when the implementation has
  -- internally failed, not due to anything in the protocol.
  GoAwayStatusUnknown Word32
  -- ^ Unrecognized by this library (not part of the SPDY spec).
  deriving (Eq, Show, Read)

-- | Flags used in a 'Headers' control frame.
data HeadersFlag =
  HeadersFlagFin
  -- ^ The enclosing frame is the last the sender will transmit on
  -- this stream, and the sender is now in the half-closed state.
  deriving (Eq, Show, Read)

instance Flag HeadersFlag where
  bit HeadersFlagFin = 0

-- | The number of bytes now free in the sender's data transfer
-- window.
newtype DeltaWindowSize =
  DeltaWindowSize Word32
  deriving (Eq, Ord, Enum, Num, Real, Integral, Show, Read)

-- | A cryptographic proof that the client has the necessary private
-- key. The format is a TLS digitally-signed element. See the spec for
-- details and references to the relevant RFCs.
newtype Proof = Proof ByteString deriving (Eq, Show, Read)

-- | A DER-encoded client certificate.
newtype Certificate = Certificate ByteString deriving (Eq, Show, Read)

-- | A collection of functions for responding to the various types of
-- frames.
data FrameHandlers a =
  FrameHandlers
  { handleDataFrame :: DataFrame ->  a
  , handleSynStreamFrame :: SPDYVersion -> SynStreamFrame -> a
  , handleSynReplyFrame :: SPDYVersion -> SynReplyFrame -> a
  , handleRstStreamFrame :: SPDYVersion -> RstStreamFrame -> a
  , handleSettingsFrame :: SPDYVersion -> SettingsFrame -> a
  , handlePingFrame :: SPDYVersion -> PingFrame -> a
  , handleGoAwayFrame :: SPDYVersion -> GoAwayFrame -> a
  , handleHeadersFrame :: SPDYVersion -> HeadersFrame -> a
  , handleWindowUpdateFrame :: SPDYVersion -> WindowUpdateFrame -> a
  , handleCredentialFrame :: SPDYVersion -> CredentialFrame -> a }

-- | Applies the correct handler from a collection to a frame.
handleFrame :: FrameHandlers a -> Frame -> a
handleFrame handlers f =
  case f of
    ADataFrame df -> handleDataFrame handlers df
    AControlFrame v cf ->
      case cf of
        ASynStreamFrame f ->
          handleSynStreamFrame handlers v f
        ASynReplyFrame f ->
          handleSynReplyFrame handlers v f
        ARstStreamFrame f ->
          handleRstStreamFrame handlers v f
        ASettingsFrame f ->
          handleSettingsFrame handlers v f
        APingFrame f ->
          handlePingFrame handlers v f
        AGoAwayFrame f ->
          handleGoAwayFrame handlers v f
        AHeadersFrame f ->
          handleHeadersFrame handlers v f
        AWindowUpdateFrame f ->
          handleWindowUpdateFrame handlers v f
        ACredentialFrame f ->
          handleCredentialFrame handlers v f

-- | A set of do-nothing frame handlers in a monad capable of IO.
defaultIOFrameHandlers :: MonadIO m => FrameHandlers (m ())
defaultIOFrameHandlers =
  FrameHandlers
  { handleDataFrame = doNothing1
  , handleSynStreamFrame = doNothing2
  , handleSynReplyFrame = doNothing2
  , handleRstStreamFrame = doNothing2
  , handleSettingsFrame = doNothing2
  , handlePingFrame = doNothing2
  , handleGoAwayFrame = doNothing2
  , handleHeadersFrame = doNothing2
  , handleWindowUpdateFrame = doNothing2
  , handleCredentialFrame = doNothing2 }
    where doNothing1 = const $ return ()
          doNothing2 = const $ const $ return ()

-- | An enumeration of frame types. This is useful for operations that
-- rely only on the kind of frame.
data FrameType = Data
               | SynStream
               | SynReply
               | RstStream
               | Settings
               | Ping
               | GoAway
               | Headers
               | WindowUpdate
               | Credential
               deriving (Eq, Ord, Show, Read)

-- | Types with an associated 'FrameType'. In particular, frames.
class WithFrameType a where
  frameTypeOf :: a -> FrameType

-- | Returns the name of a frame type, as given in the spec.
frameTypeName :: WithFrameType f => f -> String
frameTypeName f =
  case frameTypeOf f of
    Data -> "DATA"
    SynStream -> "SYN_STREAM"
    SynReply -> "SYN_REPLY"
    RstStream -> "RST_STREAM"
    Settings -> "SETTINGS"
    Ping -> "PING"
    GoAway -> "GOAWAY"
    Headers -> "HEADERS"
    WindowUpdate -> "WINDOW_UPDATE"
    Credential -> "CREDENTIAL"

instance WithFrameType Frame where
  frameTypeOf (ADataFrame d) = frameTypeOf d
  frameTypeOf (AControlFrame _ c) = frameTypeOf c

instance WithFrameType DataFrame where
  frameTypeOf = const Data

instance WithFrameType ControlFrame where
  frameTypeOf (ASynStreamFrame f) = frameTypeOf f
  frameTypeOf (ASynReplyFrame f) = frameTypeOf f
  frameTypeOf (ARstStreamFrame f) = frameTypeOf f
  frameTypeOf (ASettingsFrame f) = frameTypeOf f
  frameTypeOf (APingFrame f) = frameTypeOf f
  frameTypeOf (AGoAwayFrame f) = frameTypeOf f
  frameTypeOf (AHeadersFrame f) = frameTypeOf f
  frameTypeOf (AWindowUpdateFrame f) = frameTypeOf f
  frameTypeOf (ACredentialFrame f) = frameTypeOf f

instance WithFrameType SynStreamFrame where
  frameTypeOf = const SynStream

instance WithFrameType SynReplyFrame where
  frameTypeOf = const SynReply

instance WithFrameType RstStreamFrame where
  frameTypeOf = const RstStream

instance WithFrameType SettingsFrame where
  frameTypeOf = const Settings

instance WithFrameType PingFrame where
  frameTypeOf = const Ping

instance WithFrameType GoAwayFrame where
  frameTypeOf = const GoAway

instance WithFrameType HeadersFrame where
  frameTypeOf = const Headers

instance WithFrameType WindowUpdateFrame where
  frameTypeOf = const WindowUpdate

instance WithFrameType CredentialFrame where
  frameTypeOf = const Credential

-- | Types with an associated stream ID. In particular, frames that
-- are directed at particular streams.
class WithStream a where
  streamOf :: a -> StreamID

instance WithStream DataFrame where
  streamOf = streamID

instance WithStream SynStreamFrame where
  streamOf = synStreamNewStreamID

instance WithStream SynReplyFrame where
  streamOf = synReplyNewStreamID

instance WithStream RstStreamFrame where
  streamOf = rstStreamTermStreamID

instance WithStream HeadersFrame where
  streamOf = headersStreamID

instance WithStream WindowUpdateFrame where
  streamOf = windowUpdateStreamID

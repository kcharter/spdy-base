{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |

Data types for representing SPDY frames.

The definitions here are derived from the SPDY draft specification
published online at

<http://mbelshe.github.com/SPDY-Specification/draft-mbelshe-spdy-00.xml>

Many of the descriptive comments in this module are taken almost
word-for-word from the draft spec. This particular version uses the
draft that expires in November 2011.

-}


module Network.SPDY.Frames where

import Data.Bits (Bits)
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8

import Network.SPDY.Flags

-- | The compression dictionary to use for the zlib compression of headers.
compressionDictionary :: ByteString
compressionDictionary =
  BSC8.pack $
  "optionsgetheadpostputdeletetraceacceptaccept-charsetaccept-encodingaccept-" ++
  "languageauthorizationexpectfromhostif-modified-sinceif-matchif-none-matchi" ++
  "f-rangeif-unmodifiedsincemax-forwardsproxy-authorizationrangerefererteuser" ++
  "-agent10010120020120220320420520630030130230330430530630740040140240340440" ++
  "5406407408409410411412413414415416417500501502503504505accept-rangesageeta" ++
  "glocationproxy-authenticatepublicretry-afterservervarywarningwww-authentic" ++
  "ateallowcontent-basecontent-encodingcache-controlconnectiondatetrailertran" ++
  "sfer-encodingupgradeviawarningcontent-languagecontent-lengthcontent-locati" ++
  "oncontent-md5content-rangecontent-typeetagexpireslast-modifiedset-cookieMo" ++
  "ndayTuesdayWednesdayThursdayFridaySaturdaySundayJanFebMarAprMayJunJulAugSe" ++
  "pOctNovDecchunkedtext/htmlimage/pngimage/jpgimage/gifapplication/xmlapplic" ++
  "ation/xhtmltext/plainpublicmax-agecharset=iso-8859-1utf-8gzipdeflateHTTP/1" ++
  ".1statusversionurl" ++
  "\0"

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
  ControlFrame
  { spdyVersion :: SPDYVersion
    -- ^ The version of the SPDY protocol.
  , controlFrameDetails :: ControlFrameDetails
  } |
  -- | A data frame. Data frames carry raw data between a sender and a
  -- receiver.
  DataFrame
  { streamID :: StreamID
    -- ^ Identifies the stream to which the accompanying data belongs.
  , dataFlags :: DataFlags
    -- ^ Flags for the data frame.
  , dataBytes :: ByteString
    -- ^ The raw data.
  }
  deriving (Eq, Show, Read)

-- | Higher-level representation of the contents of the different
-- kinds of control frames.
data ControlFrameDetails =
  -- | Request to create a new stream.
  SynStream
  { synStreamFlags :: SynStreamFlags
    -- ^ Flags for stream creation.
  , newStreamID :: StreamID
    -- ^ Uniquely identifies the new stream.
  , associatedTo :: Maybe StreamID
    -- ^ Identifies a stream with which this stream is associated. If
    -- this stream is independent of all others, this must be
    -- 'Nothing'.
  , priority :: Priority
    -- ^ The priority of the new stream.
  , headerBlock :: HeaderBlock
    -- ^ A set of headers for the stream.
  } |
  -- | Acknowledges receipt of a 'SynStream' frame.
  SynReply
  { synReplyFlags :: SynReplyFlags
    -- ^ Flags for the reply.
  , newStreamID :: StreamID
    -- ^ The same stream ID as in the 'SynStream' frame.
  , headerBlock :: HeaderBlock
    -- ^ A set of headers for the stream (unsure whether this is
    -- supposed to be the exact same set of headers as in the
    -- 'SynStream' frame).
  } |
  -- | Request to abnormally terminate a stream.
  RstStream
  { termStreamID :: StreamID
    -- ^ Identifies the stream to terminate.
  , termStatus :: TerminationStatus
    -- ^ Indicates the reason why the stream is being terminated.
  } |
  -- | Exchange settings, and request or acknowledge that they have
  -- been persisted or cleared.
  Settings
  { settingsFlags :: SettingsFlags
  , settingsPairs :: [(SettingIDAndFlags, SettingValue)]
  } |
  -- | Used for estimating the minimum round-trip time from the
  -- sender. Recipients of a 'Ping' frame should send an identical
  -- frame to the sender as soon as possible. If there is other data
  -- waiting to be sent, the 'Ping' frame should take the highest
  -- priority. Each ping sent by sender should have a unique ID.
  Ping
  { pingID :: PingID
    -- ^ A unique ID for this ping.
  } |
  -- | Tells the remote endpoint to stop using this session. Once
  -- sent, the sender will not initiate any new streams on this
  -- session, and once received the receiver should not send any new
  -- requests on this session. This is intended to allow the orderly
  -- tear-down of a session.
  GoAway
  { lastGoodStreamID :: StreamID
    -- ^ The last stream ID which was accepted by the sender of this
    -- message. If no streams were accepted, must be the zero stream
    -- ID.
  , goAwayStatus :: GoAwayStatus
    -- ^ The reason for closing the session.
  } |
  -- | Augments an existing stream with additional headers.
  Headers
  { headersFlags :: HeadersFlags
  , headersStreamID :: StreamID
    -- ^ The stream ID of the stream to which the headers apply.
  , headerBlock :: HeaderBlock
    -- ^ The set of header-value pairs.
  } |
  -- | Informs the recipient that there is a (positive) change in the
  -- amount of free space in the sender's data transfer window. This
  -- frame is part of SPDY's approach to flow control. An endpoint's
  -- transfer window is the total capacity that the endpoint has for
  -- buffering raw stream data it receives.
  WindowUpdate
  { windowUpdateStreamID :: StreamID
    -- ^ The ID of the stream to which this frame applies.
  , windowUpdateDeltaWindowSize :: DeltaWindowSize
    -- ^ The additional number of bytes that the sender can transmit
    -- in addition to the remaining window size.
  }
  deriving (Eq, Show, Read)

newtype DataFlags = DataFlags Word8 deriving (Eq, Read, Show, Num, Bits, Flags)

data DataFlag =
  DataFlagFin |
  -- ^ The enclosing frame is the last one transmitted by the sender
  -- in this stream.
  DataFlagCompress
  -- ^ The data in the enclosing frame has been compressed.
  deriving (Eq, Show, Read)

instance Flag DataFlag DataFlags where
  bit DataFlagFin = 0
  bit DataFlagCompress = 1

newtype SynStreamFlags = SynStreamFlags Word8 deriving (Eq, Read, Show, Num, Bits, Flags)

-- | Flags used in the 'SynStream' frame.
data SynStreamFlag =
  SynStreamFlagFin |
  -- ^ The enclosing frame is the last to be transmitted on this
  -- stream and the sender is in the half-closed state.
  SynStreamFlagUnidirectional
  -- ^ The recipient should start in the half-closed state.
  deriving (Eq, Show, Read)

instance Flag SynStreamFlag SynStreamFlags where
  bit SynStreamFlagFin = 0
  bit SynStreamFlagUnidirectional = 1

newtype SynReplyFlags = SynReplyFlags Word8 deriving (Eq, Read, Show, Num, Bits, Flags)

-- | Flags used in the 'SynReply' frame.
data SynReplyFlag =
  SynReplyFlagFin
  -- ^ The enclosing frame is the last to be transmitted on this
  -- stream and the sender is in the half-closed state.
  deriving (Eq, Show, Read)

instance Flag SynReplyFlag SynReplyFlags where
  bit SynReplyFlagFin = 0

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
  FlowControlError |
  -- ^ The endpoint detected that its peer violated the flow control
  -- protocol.
  StreamInUse |
  -- ^ The endpoint received a 'SynReply' for a stream already open.
  StreamAlreadyClosed |
  -- ^ The endpoint received a data or 'SynReply' frame for a stream
  -- which is half-closed.
  TerminationStatusUnknown Word32
  -- ^ Some other termination status, unrecognized by this
  -- library. This is *not* part of the SPDY spec.
  deriving (Eq, Show, Read)

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
newtype StreamID = StreamID Word32 deriving (Eq, Show, Read)

-- | Extracts the raw stream ID word from a 'StreamID'.
rawStreamID :: StreamID -> Word32
rawStreamID (StreamID w) = w

-- | The priority of a stream. Legal numeric values range from @0@ to
-- @7@, @0@ being the highest priority.
newtype Priority = Priority Word8 deriving (Eq, Show, Read)

instance Ord Priority where
  compare (Priority p1) (Priority p2) =  compare p2 p1

-- | A list of header names and their corresponding values.
data HeaderBlock =
  HeaderBlock
  { headerCount :: HeaderCount
    -- ^ Number of headers in the block.
  , headerPairs :: [(HeaderName, HeaderValue)]
    -- ^ The headers and their values.
  }
  deriving (Eq, Show, Read)

-- | The number of headers (name-value pairs) in a header block.
newtype HeaderCount = HeaderCount Word32 deriving (Eq, Show, Read)

-- | A header name.
newtype HeaderName = HeaderName ByteString deriving (Eq, Show, Read)

-- | A header value.
newtype HeaderValue = HeaderValue ByteString deriving (Eq, Show, Read)

-- | A settings ID paired with a list of settings flags.
data SettingIDAndFlags =
  SettingIDAndFlags
  { settingIDFlags :: SettingIDFlags
    -- ^ The flags.
  , settingID :: SettingID
    -- ^ The setting ID itself.
  }
  deriving (Eq, Show, Read)

newtype SettingsFlags = SettingsFlags Word8 deriving (Eq, Read, Show, Num, Bits, Flags)

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

instance Flag SettingsFlag SettingsFlags where
  bit SettingsFlagClearSettings = 0

newtype SettingIDFlags = SettingIDFlags Word8 deriving (Eq, Read, Show, Num, Bits, Flags)

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

instance Flag SettingIDFlag SettingIDFlags where
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
  SettingsDownloadRetrainsRate |
  -- ^ The sender's retransmission rate (bytes retransmitted / total
  -- bytes transmitted).
  SettingsInitialWindowSize |
  -- ^ The sender's initial window size, in bytes, for new streams.
  SettingsOther Word32
  -- ^ A catch-all for other settings values that this code doesn't
  -- recognize. This is *not* part of the SPDY specification.
  deriving (Eq, Show, Read)

-- | The value for a setting.
newtype SettingValue = SettingValue Word32 deriving (Eq, Show, Read)

-- | When a client initiates a ping, it must use an odd-numbered
-- ID. When the server initiates a ping, it must use an even-numbered
-- ID. This scheme avoids accidental looping on pings, where each side
-- initiates an identical ping at the same time.
newtype PingID = PingID Word32 deriving (Eq, Show, Read)

-- | The reasons for receiving a 'GoAway' frame.
data GoAwayStatus =
  GoAwayOK |
  -- ^ This is a normal session tear-down.
  GoAwayProtocolError |
  -- ^ A generic error, used only if no more specific error applies.
  GoAwayStatusUnknown Word32
  -- ^ Unrecognized by this library (not part of the SPDY spec).
  deriving (Eq, Show, Read)

newtype HeadersFlags = HeadersFlags Word8 deriving (Eq, Read, Show, Num, Bits, Flags)

-- | Flags used in a 'Headers' control frame.
data HeadersFlag =
  HeadersFlagFin
  -- ^ The enclosing frame is the last the sender will transmit on
  -- this stream, and the sender is now in the half-closed state.
  deriving (Eq, Show, Read)

instance Flag HeadersFlag HeadersFlags where
  bit HeadersFlagFin = 0

-- | The number of bytes now free in the sender's data transfer
-- window.
newtype DeltaWindowSize = DeltaWindowSize Word32 deriving (Eq, Show, Read)

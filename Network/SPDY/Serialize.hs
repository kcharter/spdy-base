{- |

Serialization of SPDY frames to lazy 'ByteString's.
-}

module Network.SPDY.Serialize
       (rawFrameToByteString, toRawFrame, frameToByteString) where

import Blaze.ByteString.Builder
import Data.Bits (setBit, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.Word (Word8, Word16, Word32)

import Network.SPDY.Frames
import Network.SPDY.ToWord8

-- | Convert a single raw frame into a strict 'ByteString'.
rawFrameToByteString :: RawFrame -> B.ByteString
rawFrameToByteString = toByteString . rawFrameBuilder

rawFrameBuilder :: RawFrame -> Builder
rawFrameBuilder frame =
  rawHeaderBuilder (frameHeader frame) `mappend`
  fromWord8 (flagsByte frame) `mappend`
  fromWord8 plHi8 `mappend`
  fromWord16be plLo16 `mappend`
  fromByteString (payload frame)
    where pl = fromIntegral $ payloadLength frame :: Word32
          plHi8 = fromIntegral $ (pl `shiftR` 16) .&. 0xff
          plLo16 = fromIntegral $ pl .&. 0xffff

rawHeaderBuilder :: RawFrameHeader -> Builder
rawHeaderBuilder header =
  case header of
    ControlFrameHeader v ct ->
      fromWord16be (setBit (rawSPDYVersion v) 15) `mappend`
      fromWord16be ct
    DataFrameHeader sid ->
      fromWord32be (rawStreamID sid)

-- | Converts a frame into a raw frame.
toRawFrame :: Frame -> RawFrame
toRawFrame frame =
  RawFrame { frameHeader = toRawFrameHeader frame
           , flagsByte = toFlagsByte frame
           , payload = toPayload frame }

toRawFrameHeader :: Frame -> RawFrameHeader
toRawFrameHeader frame =
  case frame of
    ControlFrame v d ->
      ControlFrameHeader v $ toControlType d
    DataFrame sid _ _ ->
      DataFrameHeader sid

toControlType :: ControlFrameDetails -> Word16
toControlType details =
  case details of
    SynStream _ _ _ _ _ -> cftSynStream
    SynReply _ _ _ -> cftSynReply
    RstStream _ _ -> cftRstStream
    Settings _ _ -> cftSettings
    Ping _ -> cftPing
    GoAway _ _ -> cftPing
    Headers _ _ _ -> cftHeaders
    WindowUpdate _ _ -> cftWindowUpdate

toFlagsByte :: Frame -> Word8
toFlagsByte frame =
  case frame of
    ControlFrame _ d ->
      case d of
        SynStream f _ _ _ _ -> toWord8 f
        SynReply f _ _ -> toWord8 f
        Settings f _ -> toWord8 f
        Headers f _ _ -> toWord8 f
        _ -> 0x0
    DataFrame _ f _ -> toWord8 f


toPayload :: Frame -> ByteString
toPayload frame =
  case frame of
    ControlFrame _ d ->
      toControlPayload d
    DataFrame _ _ bs ->
      bs

toControlPayload :: ControlFrameDetails -> ByteString
toControlPayload = toByteString . toControlPayloadBuilder

toControlPayloadBuilder :: ControlFrameDetails -> Builder
toControlPayloadBuilder details =
  case details of
    SynStream _ id sid pri hb ->
      error "ni"
    SynReply _ sid hb ->
      error "ni"
    RstStream sid status ->
      error "ni"
    Settings _ pairs ->
      error "ni"
    Ping pid ->
      error "ni"
    GoAway sid status ->
      error "ni"
    Headers _ sid hb ->
      error "ni"
    WindowUpdate sid dws ->
      error "ni"

-- | Converts a frame to a 'ByteString'.
frameToByteString :: Frame -> ByteString
frameToByteString = rawFrameToByteString . toRawFrame

{- |

Serialization of SPDY frames to lazy 'ByteString's.
-}

module Network.SPDY.Serialize (rawFrameToByteString) where

import Blaze.ByteString.Builder
import Data.Bits (setBit, shiftR, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.Word (Word32)

import Network.SPDY.Frames

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

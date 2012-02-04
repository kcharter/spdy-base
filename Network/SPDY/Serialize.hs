{- |

Serialization of SPDY frames to lazy 'ByteString's.
-}

module Network.SPDY.Serialize (rawFrameToByteString) where

import Blaze.ByteString.Builder
import Data.Bits (setBit)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid

import Network.SPDY.Frames

-- | Convert a single raw frame into a strict 'ByteString'.
rawFrameToByteString :: RawFrame -> B.ByteString
rawFrameToByteString = toByteString . rawFrameBuilder

rawFrameBuilder :: RawFrame -> Builder
rawFrameBuilder frame =
  rawHeaderBuilder (frameHeader frame) `mappend`
  fromWord8 (flagsByte frame) `mappend`
  fromWord16be (fromIntegral $ payloadLength frame) `mappend`
  fromByteString (payload frame)

rawHeaderBuilder :: RawFrameHeader -> Builder
rawHeaderBuilder header =
  case header of
    ControlFrameHeader v ct ->
      fromWord16be (setBit (rawSPDYVersion v) 15) `mappend`
      fromWord16be ct
    DataFrameHeader sid ->
      fromWord32be (rawStreamID sid)

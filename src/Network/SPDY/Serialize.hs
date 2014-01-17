{-# LANGUAGE FlexibleInstances #-}

{- |

Serialization of SPDY frames to 'ByteString's.
-}

module Network.SPDY.Serialize
       (rawFrameToByteString, toRawFrame, frameToByteString) where

import Blaze.ByteString.Builder
import Codec.Zlib (Deflate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Network.SPDY.Frames
import Network.SPDY.Internal.Serialize

-- | Convert a single raw frame into a strict 'ByteString'.
rawFrameToByteString :: RawFrame -> B.ByteString
rawFrameToByteString = toByteString . rawFrameBuilder


-- | Converts a frame into a raw frame.
toRawFrame :: Deflate -> Frame -> IO RawFrame
toRawFrame deflate frame = do
  pl <- toPayload deflate frame
  return  RawFrame { frameHeader = toRawFrameHeader frame
                   , flagsByte = toFlagsByte frame
                   , payload = pl }


-- | Converts a frame to a 'ByteString'.
frameToByteString :: Deflate -> Frame -> IO ByteString
frameToByteString deflate = fmap rawFrameToByteString . toRawFrame deflate

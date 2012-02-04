module RawFrameTests where

import Test.QuickCheck

import Network.SPDY.Deserialize
import Network.SPDY.Frames
import Network.SPDY.Serialize

import Instances

run :: IO ()
run = quickCheck prop_deserializeSerialize

prop_deserializeSerialize :: RawFrame -> Bool
prop_deserializeSerialize frame =
  either (const False) (frame ==) $
  rawFrameFromByteString (rawFrameToByteString frame)

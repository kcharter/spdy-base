module RawFrameTests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Network.SPDY.Deserialize
import Network.SPDY.Frames
import Network.SPDY.Serialize

import Instances

test :: Test
test = testGroup "Raw frame tests" [
  testProperty "serialize-deserialize" prop_deserializeSerialize
  ]

prop_deserializeSerialize :: RawFrame -> Bool
prop_deserializeSerialize frame =
  either (const False) (frame ==) $
  rawFrameFromByteString (rawFrameToByteString frame)

module FrameTests where

import Control.Applicative ((<$>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Property (morallyDubiousIOProperty)
import Codec.Zlib (initDeflateWithDictionary,
                   initInflateWithDictionary,
                   defaultWindowBits)

import Network.SPDY.Compression
import Network.SPDY.Deserialize
import Network.SPDY.Frames
import Network.SPDY.Serialize

import Instances ()


test :: Test
test = testGroup "Frame tests" [
  testProperty "to-raw-frame-and-back" prop_toRawFrameAndBack
  ]

prop_toRawFrameAndBack :: Frame -> Gen Prop
prop_toRawFrameAndBack frame =
  morallyDubiousIOProperty $
  either (const False) (frame ==) <$>
  (toFrame' =<< toRawFrame' frame)

toRawFrame' :: Frame -> IO RawFrame
toRawFrame' frame = do
  deflate <- initDeflateWithDictionary defaultCompressionLevel compressionDictionary defaultWindowBits
  toRawFrame deflate frame


toFrame' :: RawFrame -> IO (Either String Frame)
toFrame' rawFrame = do
  inflate <- initInflateWithDictionary defaultWindowBits compressionDictionary
  toFrame inflate rawFrame

defaultCompressionLevel :: Int
defaultCompressionLevel = 6 -- from the official zlib manual

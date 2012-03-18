module FrameTests where

import Control.Applicative ((<$>))
import Data.Monoid
import Test.Framework (Test, testGroup, plusTestOptions, topt_maximum_generated_tests)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Property (morallyDubiousIOProperty)
import Codec.Zlib (initDeflateWithDictionary,
                   initInflateWithDictionary)

import Network.SPDY.Compression
import Network.SPDY.Deserialize
import Network.SPDY.Frames
import Network.SPDY.Serialize

import Instances ()


test :: Test
test = testGroup "Frame tests" [
  -- Because there is a large variety of frame types, we tell
  -- test-framework to use up to 1000 test iterations for this
  -- test. In practice, we'll run 1000 test iterations, despite the
  -- name 'topt_maximum_generated_tests'
  plusTestOptions opts $ testProperty "to-raw-frame-and-back" prop_toRawFrameAndBack
  ]
  where opts = mempty { topt_maximum_generated_tests = Just 1000 }

prop_toRawFrameAndBack :: Frame -> Gen Prop
prop_toRawFrameAndBack frame =
  morallyDubiousIOProperty $
  either (const False) (frame ==) <$>
  (toFrame' =<< toRawFrame' frame)

toRawFrame' :: Frame -> IO RawFrame
toRawFrame' frame = do
  deflate <- initDeflateWithDictionary defaultCompressionLevel compressionDictionary defaultSPDYWindowBits
  toRawFrame deflate frame


toFrame' :: RawFrame -> IO (Either String Frame)
toFrame' rawFrame = do
  inflate <- initInflateWithDictionary defaultSPDYWindowBits compressionDictionary
  toFrame inflate rawFrame

defaultCompressionLevel :: Int
defaultCompressionLevel = 6 -- from the official zlib manual

module Instances where

import Control.Applicative
import Data.ByteString (ByteString, pack)
import Data.Word
import Test.QuickCheck

import Network.SPDY.Flags
import Network.SPDY.Frames

instance Arbitrary RawFrame where
  arbitrary = do
    pl <- arbitrary
    (RawFrame <$>
     arbitrary <*>
     arbitrary <*>
     payloadBytes pl)

instance Arbitrary RawFrameHeader where
  arbitrary =
    oneof [ControlFrameHeader <$> arbitrary <*> arbitrary,
           DataFrameHeader <$> arbitrary]

instance Arbitrary SPDYVersion where
  arbitrary = SPDYVersion . fromIntegral <$> choose (0, 8 :: Int)

instance Arbitrary StreamID where
  arbitrary = StreamID . fromIntegral <$> choose (0, 0x7fffffff :: Int)

instance Arbitrary DataLength where
  arbitrary = sized $ \n ->
    DataLength . fromIntegral <$> choose (0, min n 4096 :: Int)

payloadBytes :: DataLength -> Gen ByteString
payloadBytes = bytes . fromIntegral

bytes :: Int -> Gen ByteString
bytes n = pack <$> vectorOf n arbitrary

instance Arbitrary ByteString where
  arbitrary = sized bytes

instance Arbitrary Frame where
  arbitrary = DataFrame <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DataFlags where
  arbitrary = packFlags <$> arbitrary

instance Arbitrary DataFlag where
  arbitrary = oneof [return DataFlagFin, return DataFlagCompress]

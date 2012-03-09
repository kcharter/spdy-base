{-# LANGUAGE FlexibleInstances #-}

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

uptoBytes :: Int -> Gen ByteString
uptoBytes n = pack <$> resize n (listOf arbitrary)

instance Arbitrary ByteString where
  arbitrary = sized uptoBytes

instance Arbitrary Frame where
  arbitrary =
    oneof [ DataFrame <$> arbitrary <*> arbitrary <*> arbitrary
          , ControlFrame <$> arbitrary <*> arbitrary ]

-- it's unfortunate to have to repeat the code for every XFlags
-- instance declaration, but I cannot figure out a way to have a
-- single instance declaration without resorting to
-- UndecidableInstances and consequently getting a bunch of
-- overlapping instance errors

instance Arbitrary DataFlags where
  arbitrary = packFlags <$> arbitrary

instance Arbitrary DataFlag where
  arbitrary = oneof [return DataFlagFin, return DataFlagCompress]

instance Arbitrary ControlFrameDetails where
  arbitrary =
    oneof [ SynStream <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          , SynReply <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Arbitrary SynStreamFlags where
  arbitrary = packFlags <$> arbitrary

instance Arbitrary SynStreamFlag where
  arbitrary = oneof $ map return [ SynStreamFlagFin
                                 , SynStreamFlagUnidirectional ]

instance Arbitrary Priority where
  arbitrary = Priority . fromIntegral <$> choose (0,7 :: Int)

instance Arbitrary Slot where
  arbitrary = Slot . fromIntegral <$> choose (0, 255 :: Int)

instance Arbitrary HeaderBlock where
  arbitrary = do
    pairs <- listOf arbitrary
    return $ HeaderBlock pairs

-- although we don't need this instance for the instance for
-- HeaderBlock, this allows a stand-alone test of the builder and
-- attoparsec parser.
instance Arbitrary HeaderCount where
  arbitrary = HeaderCount <$> (fromIntegral <$> choose (0, 1024 :: Int))

instance Arbitrary HeaderName where
  arbitrary = HeaderName <$> shortBytes

instance Arbitrary HeaderValue where
  arbitrary = HeaderValue <$> shortBytes

shortBytes :: Gen ByteString
shortBytes = resize 32 arbitrary


instance Arbitrary SynReplyFlags where
  arbitrary = packFlags <$> arbitrary

instance Arbitrary SynReplyFlag where
  arbitrary = return SynReplyFlagFin

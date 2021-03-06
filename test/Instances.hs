module Instances where

import Control.Applicative
import Data.ByteString (ByteString, pack)
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
    oneof [ ADataFrame <$> arbitrary
          , AControlFrame <$> arbitrary <*> arbitrary ]

instance Arbitrary DataFrame where
  arbitrary = DataFrame <$> arbitrary <*> arbitrary <*> arbitrary

instance (Flag f, Arbitrary f) => Arbitrary (Flags f) where
  arbitrary = packFlags <$> arbitrary

instance Arbitrary DataFlag where
  arbitrary = oneof [return DataFlagFin, return DataFlagCompress]

instance Arbitrary ControlFrame where
  arbitrary =
    oneof [ ASynStreamFrame <$> arbitrary
          , ASynReplyFrame <$> arbitrary
          , ARstStreamFrame <$> arbitrary
          , ASettingsFrame <$> arbitrary
          , APingFrame <$> arbitrary
          , AGoAwayFrame <$> arbitrary
          , AHeadersFrame <$> arbitrary
          , AWindowUpdateFrame <$> arbitrary
          , ACredentialFrame <$> arbitrary ]

instance Arbitrary SynStreamFrame where
  arbitrary = SynStreamFrame <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

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

instance Arbitrary SynReplyFrame where
  arbitrary =  SynReplyFrame <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SynReplyFlag where
  arbitrary = return SynReplyFlagFin

instance Arbitrary RstStreamFrame where
  arbitrary = RstStreamFrame <$> arbitrary <*> arbitrary

instance Arbitrary TerminationStatus where
  arbitrary =
    frequency [ (length fixedValues, elements fixedValues)
              , (1, fmap (TerminationStatusUnknown . fromIntegral) $ choose (12, 1024 :: Int)) ]
      where fixedValues = [ ProtocolError,
                            InvalidStream,
                            RefusedStream,
                            UnsupportedVersion,
                            Cancel,
                            InternalError,
                            FlowControlError,
                            StreamInUse,
                            StreamAlreadyClosed,
                            InvalidCredentials,
                            FrameTooLarge ]

instance Arbitrary SettingsFrame where
  arbitrary = SettingsFrame <$> arbitrary <*> arbitrary

instance Arbitrary SettingsFlag where
  arbitrary = elements [ SettingsFlagClearSettings ]

instance Arbitrary SettingIDAndFlags where
  arbitrary = SettingIDAndFlags <$> arbitrary <*> arbitrary

instance Arbitrary SettingIDFlag where
  arbitrary = elements [ SettingIDFlagPersistValue
                       , SettingIDFlagPersisted ]

instance Arbitrary SettingID where
  arbitrary =
    frequency [(length fixedValues, elements fixedValues)
              ,(1, (SettingsOther . fromIntegral) <$> choose (length fixedValues + 1, 1023))]
      where fixedValues = [  SettingsUploadBandwidth,
                             SettingsDownloadBandwidth,
                             SettingsRoundTripTime,
                             SettingsMaxConcurrentStreams,
                             SettingsCurrentCWND,
                             SettingsDownloadRetransRate,
                             SettingsInitialWindowSize,
                             SettingsClientCertificateVectorSize ]

instance Arbitrary SettingValue where
  arbitrary = (SettingValue . fromIntegral) <$> choose (0, 2 ^ 24 - 1 :: Int)

instance Arbitrary PingFrame where
  arbitrary = PingFrame <$> arbitrary

instance Arbitrary PingID where
  arbitrary = (PingID . fromIntegral) <$> choose (0, 2 ^ 29 - 1 :: Int)

instance Arbitrary GoAwayFrame where
  arbitrary = GoAwayFrame <$> arbitrary <*> arbitrary

instance Arbitrary GoAwayStatus where
  arbitrary =
    frequency [ (length fixedValues, elements fixedValues)
              , (1, (GoAwayStatusUnknown . fromIntegral) <$> choose (12, 2 ^ 29 - 1 :: Int)) ]
      where fixedValues = [ GoAwayOK,
                            GoAwayProtocolError,
                            GoAwayInternalError ]

instance Arbitrary HeadersFrame where
  arbitrary = HeadersFrame <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary HeadersFlag where
  arbitrary = return HeadersFlagFin

instance Arbitrary WindowUpdateFrame where
  arbitrary = WindowUpdateFrame <$> arbitrary <*> arbitrary

instance Arbitrary DeltaWindowSize where
  arbitrary = (DeltaWindowSize . fromIntegral) <$> choose (0, 2 ^ 31 - 1 :: Integer)

instance Arbitrary CredentialFrame where
  arbitrary = CredentialFrame <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Slot16 where
  arbitrary = (Slot16 . fromIntegral) <$> choose (0, 2 ^ 16 - 1 :: Int)

instance Arbitrary Proof where
  arbitrary = Proof <$> arbitrary

instance Arbitrary Certificate where
  arbitrary = Certificate <$> arbitrary

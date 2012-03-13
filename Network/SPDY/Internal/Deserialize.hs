module Network.SPDY.Internal.Deserialize where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Attoparsec.ByteString (Parser, anyWord8)
import qualified Data.Attoparsec.ByteString as AP
import Data.Bits (shiftR, shiftL, testBit, clearBit, (.|.))
import Data.ByteString (ByteString)
import Data.Word

import Network.SPDY.Frames
import Network.SPDY.Flags

parseFrameHeader :: Parser RawFrameHeader
parseFrameHeader = fromBytes <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8
  where fromBytes b1 b2 b3 b4 =
          if testBit b1 controlBit
          then
            ControlFrameHeader
            (SPDYVersion $ netWord16 (clearBit b1 controlBit) b2)
            (netWord16 b3 b4)
          else
            DataFrameHeader $ StreamID $ netWord32 b1 b2 b3 b4
        controlBit = 7

parseDataLength :: Parser DataLength
parseDataLength = fmap DataLength anyWord24

anyWord32 :: Parser Word32
anyWord32 = netWord32 <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8

anyWord24 :: Parser Word32
anyWord24 = fromBytes <$> anyWord8 <*> anyWord8 <*> anyWord8
  where fromBytes hi mid lo =
          shiftL (fromIntegral hi) 16 .|. shiftL (fromIntegral mid) 8 .|. fromIntegral lo

anyWord16 :: Parser Word16
anyWord16 = netWord16 <$> anyWord8 <*> anyWord8

netWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
netWord32 hi mid1 mid2 lo =
  shiftL (fromIntegral hi) 24 .|. shiftL (fromIntegral mid1) 16 .|.
  shiftL (fromIntegral mid2) 8 .|. fromIntegral lo

netWord16 :: Word8 -> Word8 -> Word16
netWord16 hi lo = shiftL (fromIntegral hi) 8 .|. fromIntegral lo


parseSynStreamContent :: Parser (StreamID, Maybe StreamID, Priority, Slot, ByteString)
parseSynStreamContent = do
  sid <- parseStreamID
  asid <- parseMaybeStreamID
  pri <- parsePriority
  slot <- parseSlot
  headerBytes <- parseRest
  return (sid, asid, pri, slot, headerBytes)

parseSynReplyContent :: Parser (StreamID, ByteString)
parseSynReplyContent = do
  sid <- parseStreamID
  headerBytes <- parseRest
  return (sid, headerBytes)

parseRstStreamContent :: Parser (StreamID, TerminationStatus)
parseRstStreamContent = (,) <$> parseStreamID <*> parseTerminationStatus

parseGoAwayContent :: Parser (StreamID, GoAwayStatus)
parseGoAwayContent = (,) <$> parseStreamID <*> parseGoAwayStatus

parseStreamID :: Parser StreamID
parseStreamID = StreamID <$> anyWord32

parseMaybeStreamID :: Parser (Maybe StreamID)
parseMaybeStreamID = nothingIfZero <$> anyWord32
  where nothingIfZero w | w == 0x0 = Nothing
                        | otherwise = Just $ StreamID w

parsePriority :: Parser Priority
parsePriority = (Priority . (`shiftR` 5)) <$> anyWord8

parseSlot :: Parser Slot
parseSlot = Slot <$> anyWord8

parseHeaderBlock :: Parser HeaderBlock
parseHeaderBlock = do
  hc <- parseHeaderCount
  headerPairs <- replicateM (fromIntegral hc) parsePair
  return $ HeaderBlock headerPairs
  where parsePair = (,) <$> parseHeaderName <*> parseHeaderValue

parseHeaderCount :: Parser HeaderCount
parseHeaderCount = HeaderCount <$> anyWord32

parseHeaderName :: Parser HeaderName
parseHeaderName = HeaderName <$> parseLengthAndBytes

parseHeaderValue :: Parser HeaderValue
parseHeaderValue = HeaderValue <$> parseLengthAndBytes

parseLengthAndBytes :: Parser ByteString
parseLengthAndBytes = do
  len <- fmap fromIntegral $ anyWord32
  AP.take len

parseTerminationStatus :: Parser TerminationStatus
parseTerminationStatus =
  fmap toStatus anyWord32
    where toStatus w | w == tsProtocolError = ProtocolError
                     | w == tsInvalidStream = InvalidStream
                     | w == tsRefusedStream = RefusedStream
                     | w == tsUnsupportedVersion = UnsupportedVersion
                     | w == tsCancel = Cancel
                     | w == tsInternalError = InternalError
                     | w == tsFlowControlError = FlowControlError
                     | w == tsStreamInUse = StreamInUse
                     | w == tsStreamAlreadyClosed = StreamAlreadyClosed
                     | w == tsInvalidCredentials = InvalidCredentials
                     | w == tsFrameTooLarge = FrameTooLarge
                     | otherwise = TerminationStatusUnknown w

parseSettingPairs :: Parser [(SettingIDAndFlags, SettingValue)]
parseSettingPairs = do
  n <- fmap fromIntegral anyWord32
  replicateM n ((,) <$> parseSettingIDAndFlags <*> parseSettingValue)

parseSettingIDAndFlags :: Parser SettingIDAndFlags
parseSettingIDAndFlags =
  SettingIDAndFlags <$> parseSettingIDFlags <*> parseSettingID

parseSettingIDFlags :: Parser (Flags SettingIDFlag)
parseSettingIDFlags = Flags <$> anyWord8

parseSettingID :: Parser SettingID
parseSettingID =
  fmap toID anyWord24
    where toID w | w == stidSettingsUploadBandwidth = SettingsUploadBandwidth
                 | w == stidSettingsDownloadBandwidth = SettingsDownloadBandwidth
                 | w == stidSettingsRoundTripTime = SettingsRoundTripTime
                 | w == stidSettingsMaxConcurrentStreams = SettingsMaxConcurrentStreams
                 | w == stidSettingsCurrentCWND = SettingsCurrentCWND
                 | w == stidSettingsDownloadRetransRate = SettingsDownloadRetransRate
                 | w == stidSettingsInitialWindowSize = SettingsInitialWindowSize
                 | w == stidSettingsClientCertificateVectorSize = SettingsClientCertificateVectorSize
                 | otherwise = SettingsOther w

parseSettingValue :: Parser SettingValue
parseSettingValue = SettingValue <$> anyWord32

parsePingID :: Parser PingID
parsePingID = PingID <$> anyWord32

parseGoAwayStatus :: Parser GoAwayStatus
parseGoAwayStatus =
  fmap toStatus anyWord32
    where toStatus w | w == gsGoAwayOK = GoAwayOK
                     | w == gsGoAwayProtocolError = GoAwayProtocolError
                     | w == gsGoAwayInternalError = GoAwayInternalError
                     | otherwise = GoAwayStatusUnknown w

parseRest :: Parser ByteString
parseRest = AP.takeByteString

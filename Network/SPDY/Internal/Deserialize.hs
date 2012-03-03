module Network.SPDY.Internal.Deserialize where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Attoparsec.ByteString (Parser, anyWord8)
import qualified Data.Attoparsec.ByteString as AP
import Data.Bits (shiftR, shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Word

import Network.SPDY.Frames

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


parseSynStreamContent :: Parser (StreamID, Maybe StreamID, Priority, ByteString)
parseSynStreamContent = do
  sid <- parseStreamID
  asid <- parseMaybeStreamID
  pri <- parsePriority
  headerBytes <- parseRest
  return (sid, asid, pri, headerBytes)

parseStreamID :: Parser StreamID
parseStreamID = StreamID <$> anyWord32

parseMaybeStreamID :: Parser (Maybe StreamID)
parseMaybeStreamID = nothingIfZero <$> anyWord32
  where nothingIfZero w | w == 0x0 = Nothing
                        | otherwise = Just $ StreamID w

parsePriority :: Parser Priority
parsePriority = (Priority . (`shiftR` 5)) <$> anyWord8

parseHeaderBlock :: Parser HeaderBlock
parseHeaderBlock = do
  hc <- parseHeaderCount
  headerPairs <- replicateM (fromIntegral hc) parsePair
  return $ HeaderBlock hc headerPairs
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

parseRest :: Parser ByteString
parseRest = AP.takeByteString

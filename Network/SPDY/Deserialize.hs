{-|
Convert streams of bytes into SPDY frames.
-}
module Network.SPDY.Deserialize (rawFrameFromByteString,
                                 parseRawFrame) where

import Control.Applicative
import Data.Attoparsec.ByteString (Parser, anyWord8)
import qualified Data.Attoparsec.ByteString as AP
import Data.Bits ((.|.), (.&.), shiftL, testBit, clearBit)
import Data.ByteString (ByteString)
import Data.Word

import Network.SPDY.Frames (RawFrame(RawFrame),
                            RawFrameHeader(..),
                            DataLength(..),
                            SPDYVersion(..),
                            StreamID(..))

-- | Parse a raw frame from a strict 'ByteString'.
rawFrameFromByteString :: ByteString -> Either String RawFrame
rawFrameFromByteString = AP.parseOnly parseRawFrame

-- | An attoparsec parser for a raw frame. This is exposed so clients
-- can adapt to a variety of representations for the input; for
-- example, lazy byte strings, lazy I//O, some form of iteratee I//O,
-- conduits, and so on.
parseRawFrame :: Parser RawFrame
parseRawFrame = do
  header  <- parseFrameHeader
  flags   <- anyWord8
  payloadLength <- parseDataLength
  payload <- AP.take $ fromIntegral payloadLength
  return $ RawFrame header flags payloadLength payload

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

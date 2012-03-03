{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Convert streams of bytes into SPDY frames.
-}
module Network.SPDY.Deserialize (rawFrameFromByteString,
                                 parseRawFrame) where

import Blaze.ByteString.Builder
import Codec.Zlib (Inflate, withInflateInput, flushInflate)
import Control.Applicative
import Control.Monad.Error
import Data.Attoparsec.ByteString (Parser, anyWord8)
import qualified Data.Attoparsec.ByteString as AP
import Data.Bits ((.|.), (.&.), shiftL, shiftR, testBit, clearBit)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, modifyIORef, readIORef)
import Data.Monoid

import Data.Word

import Network.SPDY.Frames

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
  return $ RawFrame header flags payload

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


-- | Converts a raw frame into the corresponding processed frame,
-- given a zlib 'Inflate' decompression context.
toFrame :: Inflate -> RawFrame -> IO Frame
toFrame inflate raw =
  either fail return =<< (runErrorT $ runFrameParser $ parseFrame inflate raw)


parseFrame :: Inflate -> RawFrame -> FrameParser Frame
parseFrame inflate rawFrame =
  case frameHeader rawFrame of
    ControlFrameHeader v ctype ->
      fmap (ControlFrame v) $ parseControlFrameDetails inflate ctype flags pl
    DataFrameHeader sid ->
      return $ DataFrame sid (DataFlags flags) pl
    where flags = flagsByte rawFrame
          pl = payload rawFrame

parseControlFrameDetails :: Inflate -> Word16 -> Word8 -> ByteString
                            -> FrameParser ControlFrameDetails
parseControlFrameDetails inflate ctype flags pl
  | ctype == cftSynStream = do
      (sid, asid, pri, compressedHeaders) <- parsePayload parseSynStreamContent pl
      headerBytes <- toByteString <$> (liftIO $ decompress inflate compressedHeaders)
      headerBlock <- parsePayload parseHeaderBlock headerBytes
      return $ SynStream (SynStreamFlags flags) sid asid pri headerBlock
  | ctype == cftSynReply =
      error "ni"
  | ctype == cftRstStream =
      error "ni"
  | ctype == cftSettings =
      error "ni"
  | ctype == cftPing =
      error "ni"
  | ctype == cftGoAway =
      error "ni"
  | ctype == cftHeaders =
      error "ni"
  | ctype == cftWindowUpdate =
      error "ni"
  | otherwise =
      throwError $ "Illegal SPDY frame type '" ++ show ctype ++ "'"

parsePayload :: Parser a -> ByteString -> FrameParser a
parsePayload p bs =
  either throwError return $ AP.parseOnly p bs

-- TODO: this is almost identical to 'compress' in the 'Serialize' module. Factor out common code.
decompress :: Inflate -> ByteString -> IO Builder
decompress inflate bs = do
  bref <- newIORef mempty
  let popper = (maybe (return ()) addBS =<<)
      addBS dbs = modifyIORef bref (`mappend` fromByteString dbs)
  withInflateInput inflate bs popper
  flushInflate inflate
  readIORef bref

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

newtype FrameParser a =
  FrameParser { runFrameParser :: ErrorT String IO a }
  deriving (Functor, Monad, Applicative, MonadError String, MonadIO)

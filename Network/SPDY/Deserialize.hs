{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Convert streams of bytes into SPDY frames.
-}
module Network.SPDY.Deserialize (rawFrameFromByteString,
                                 parseRawFrame,
                                 toFrame) where

import Blaze.ByteString.Builder
import Codec.Zlib (Inflate, withInflateInput, flushInflate)
import Control.Applicative
import Control.Monad.Error
import Data.Attoparsec.ByteString (Parser, anyWord8)
import qualified Data.Attoparsec.ByteString as AP
import Data.Bits (testBit, clearBit)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, modifyIORef, readIORef)
import Data.Monoid

import Data.Word

import Network.SPDY.Frames
import Network.SPDY.Internal.Deserialize

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

-- | Converts a raw frame into the corresponding processed frame,
-- given a zlib 'Inflate' decompression context.
toFrame :: Inflate -> RawFrame -> IO (Either String Frame)
toFrame inflate =
  runErrorT . runFrameParser . parseFrame inflate


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

-- | Parse all of the content of a byte string using a given parser.
parsePayload :: Parser a -> ByteString -> FrameParser a
parsePayload p =
  either throwError return . AP.parseOnly p'
    where p' = do r <- p; AP.endOfInput; return r

-- TODO: this is almost identical to 'compress' in the 'Serialize' module. Factor out common code.
decompress :: Inflate -> ByteString -> IO Builder
decompress inflate bs = do
  bref <- newIORef mempty
  let popper = (maybe (return ()) addBS =<<)
      addBS dbs = modifyIORef bref (`mappend` fromByteString dbs)
  withInflateInput inflate bs popper
  flushInflate inflate
  readIORef bref

newtype FrameParser a =
  FrameParser { runFrameParser :: ErrorT String IO a }
  deriving (Functor, Monad, Applicative, MonadError String, MonadIO)

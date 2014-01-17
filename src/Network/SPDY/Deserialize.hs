{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Convert streams of bytes into SPDY frames.
-}
module Network.SPDY.Deserialize (rawFrameFromByteString,
                                 parseRawFrame,
                                 toFrame) where

import Blaze.ByteString.Builder
import Control.Applicative
import Control.Monad.Error
import Data.Attoparsec.ByteString (Parser, anyWord8)
import qualified Data.Attoparsec.ByteString as AP
import Data.ByteString (ByteString)

import Data.Word

import Network.SPDY.Compression (Inflate, decompress)
import Network.SPDY.Flags
import Network.SPDY.Frames
import Network.SPDY.Internal.Constants
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


-- | Converts a raw frame into the corresponding processed frame,
-- given a zlib 'Inflate' decompression context.
toFrame :: Inflate -> RawFrame -> IO (Either String Frame)
toFrame inflate =
  runErrorT . runFrameParser . parseFrame inflate


parseFrame :: Inflate -> RawFrame -> FrameParser Frame
parseFrame inflate rawFrame =
  case frameHeader rawFrame of
    ControlFrameHeader v ctype ->
      fmap (AControlFrame v) $ parseControlFrame inflate ctype flags pl
    DataFrameHeader sid ->
      return $ ADataFrame $ DataFrame sid (Flags flags) pl
    where flags = flagsByte rawFrame
          pl = payload rawFrame

parseControlFrame :: Inflate -> Word16 -> Word8 -> ByteString
                     -> FrameParser ControlFrame
parseControlFrame inflate ctype flags pl
  | ctype == cftSynStream = do
      (sid, asid, pri, slot, compressedHeaders) <- parsePayload parseSynStreamContent pl
      headerBytes <- toByteString <$> (liftIO $ decompress inflate compressedHeaders)
      headerBlock <- parsePayload parseHeaderBlock headerBytes
      return $ ASynStreamFrame $ SynStreamFrame (Flags flags) sid asid pri slot headerBlock
  | ctype == cftSynReply = do
      (sid, compressedHeaders) <- parsePayload parseSynReplyContent pl
      headerBytes <- toByteString <$> (liftIO $ decompress inflate compressedHeaders)
      headerBlock <- parsePayload parseHeaderBlock headerBytes
      return $ ASynReplyFrame $ SynReplyFrame (Flags flags) sid headerBlock
  | ctype == cftRstStream =
      (ARstStreamFrame . uncurry RstStreamFrame) <$> parsePayload parseRstStreamContent pl
  | ctype == cftSettings =
      (ASettingsFrame . SettingsFrame (Flags flags)) <$> parsePayload parseSettingPairs pl
  | ctype == cftPing =
      (APingFrame . PingFrame) <$> parsePayload parsePingID pl
  | ctype == cftGoAway =
      (AGoAwayFrame . uncurry GoAwayFrame) <$> parsePayload parseGoAwayContent pl
  | ctype == cftHeaders =
      (AHeadersFrame . uncurry (HeadersFrame (Flags flags))) <$> parsePayload parseHeadersContent pl
  | ctype == cftWindowUpdate =
      (AWindowUpdateFrame . uncurry WindowUpdateFrame) <$> parsePayload parseWindowUpdateContent pl
  | ctype == cftCredential =
      (\(slot, proof, certs) -> ACredentialFrame $ CredentialFrame slot proof certs) <$>
      parsePayload parseCredentialContent pl
  | otherwise =
      throwError $ "Illegal SPDY frame type '" ++ show ctype ++ "'"

-- | Parse all of the content of a byte string using a given parser.
parsePayload :: Parser a -> ByteString -> FrameParser a
parsePayload p =
  either throwError return . AP.parseOnly p'
    where p' = do r <- p; AP.endOfInput; return r

newtype FrameParser a =
  FrameParser { runFrameParser :: ErrorT String IO a }
  deriving (Functor, Monad, Applicative, MonadError String, MonadIO)

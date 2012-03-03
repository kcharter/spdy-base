{-# LANGUAGE FlexibleInstances #-}

module Network.SPDY.Internal.Serialize where


import Blaze.ByteString.Builder
import Codec.Zlib (Deflate, withDeflateInput, flushDeflate)
import Data.Bits (setBit, shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef (newIORef, modifyIORef, readIORef)
import Data.List (foldl')
import Data.Monoid
import Data.Word (Word8, Word16, Word32)

import Network.SPDY.Frames
import Network.SPDY.Internal.ToWord8

rawFrameBuilder :: RawFrame -> Builder
rawFrameBuilder frame =
  rawHeaderBuilder (frameHeader frame) `mappend`
  fromWord8 (flagsByte frame) `mappend`
  fromWord8 plHi8 `mappend`
  fromWord16be plLo16 `mappend`
  fromByteString (payload frame)
    where pl = fromIntegral $ payloadLength frame :: Word32
          plHi8 = fromIntegral $ (pl `shiftR` 16) .&. 0xff
          plLo16 = fromIntegral $ pl .&. 0xffff

rawHeaderBuilder :: RawFrameHeader -> Builder
rawHeaderBuilder header =
  case header of
    ControlFrameHeader v ct ->
      fromWord16be (setBit (rawSPDYVersion v) 15) `mappend`
      fromWord16be ct
    DataFrameHeader sid ->
      fromWord32be (rawStreamID sid)

toRawFrameHeader :: Frame -> RawFrameHeader
toRawFrameHeader frame =
  case frame of
    ControlFrame v d ->
      ControlFrameHeader v $ toControlType d
    DataFrame sid _ _ ->
      DataFrameHeader sid

toControlType :: ControlFrameDetails -> Word16
toControlType details =
  case details of
    SynStream _ _ _ _ _ -> cftSynStream
    SynReply _ _ _ -> cftSynReply
    RstStream _ _ -> cftRstStream
    Settings _ _ -> cftSettings
    Ping _ -> cftPing
    GoAway _ _ -> cftGoAway
    Headers _ _ _ -> cftHeaders
    WindowUpdate _ _ -> cftWindowUpdate

toFlagsByte :: Frame -> Word8
toFlagsByte frame =
  case frame of
    ControlFrame _ d ->
      case d of
        SynStream f _ _ _ _ -> toWord8 f
        SynReply f _ _ -> toWord8 f
        Settings f _ -> toWord8 f
        Headers f _ _ -> toWord8 f
        _ -> 0x0
    DataFrame _ f _ -> toWord8 f


toPayload :: Deflate -> Frame -> IO ByteString
toPayload deflate frame =
  case frame of
    ControlFrame _ d ->
      toControlPayload deflate d
    DataFrame _ _ bs ->
      return bs

toControlPayload :: Deflate -> ControlFrameDetails -> IO ByteString
toControlPayload deflate = fmap toByteString . toControlPayloadBuilder deflate

toControlPayloadBuilder :: Deflate -> ControlFrameDetails -> IO Builder
toControlPayloadBuilder deflate details =
  case details of
    SynStream _ id sid pri hb ->
      fmap (toBuilder id `mappend`
            toBuilder sid `mappend`
            toBuilder pri `mappend`) $ compressHeaderBlock deflate hb
    SynReply _ sid hb ->
      error "ni"
    RstStream sid status ->
      error "ni"
    Settings _ pairs ->
      error "ni"
    Ping pid ->
      error "ni"
    GoAway sid status ->
      error "ni"
    Headers _ sid hb ->
      error "ni"
    WindowUpdate sid dws ->
      error "ni"


compressHeaderBlock :: Deflate -> HeaderBlock -> IO Builder
compressHeaderBlock deflate hb =
  compress deflate $ toByteString $ toBuilder hb

compress :: Deflate -> ByteString -> IO Builder
compress deflate bs = do
  bref <- newIORef mempty
  let popper mbsIO = do
        mbs <- mbsIO
        maybe addEmpty addBS mbs
      addEmpty = return ()
      addBS bs = modifyIORef bref (`mappend` fromByteString bs)
  withDeflateInput deflate bs popper
  flushDeflate deflate popper
  readIORef bref

class ToBuilder a where
  toBuilder :: a -> Builder

instance ToBuilder StreamID where
  toBuilder (StreamID w) = fromWord32be w

instance ToBuilder (Maybe StreamID) where
  toBuilder = maybe (fromWord32be 0x0) toBuilder

instance ToBuilder Priority where
  toBuilder (Priority w) = fromWord8 (w `shiftL` 5)

instance ToBuilder HeaderBlock where
  toBuilder hb =
    toBuilder (headerCount hb) `mappend` headerPairsBuilder (headerPairs hb)

instance ToBuilder HeaderCount where
  toBuilder (HeaderCount w) = fromWord32be w

instance ToBuilder HeaderName where
  toBuilder (HeaderName bs) = lengthAndBytes bs

instance ToBuilder HeaderValue where
  toBuilder (HeaderValue bs) = lengthAndBytes bs

lengthAndBytes :: ByteString -> Builder
lengthAndBytes bs = fromWord32be (fromIntegral $ B.length bs) `mappend` fromByteString bs

headerPairsBuilder :: [(HeaderName, HeaderValue)] -> Builder
headerPairsBuilder =
  foldl' mappend mempty . map fromPair
    where fromPair (name, val) = toBuilder name `mappend` toBuilder val

instance ToBuilder PingID where
  toBuilder (PingID w) = fromWord32be w

instance ToBuilder DeltaWindowSize where
  toBuilder (DeltaWindowSize w) = fromWord32be w
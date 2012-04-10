{-# LANGUAGE FlexibleInstances #-}

module Network.SPDY.Internal.Serialize where


import Blaze.ByteString.Builder
import Data.Bits (setBit, shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (foldl')
import Data.Monoid
import Data.Word (Word8, Word16, Word32)

import Network.SPDY.Compression (Deflate, compress)
import Network.SPDY.Flags
import Network.SPDY.Frames
import Network.SPDY.Internal.Constants
import Network.SPDY.Internal.ToWord8

rawFrameBuilder :: RawFrame -> Builder
rawFrameBuilder frame =
  rawHeaderBuilder (frameHeader frame) `mappend`
  fromWord8 (flagsByte frame) `mappend`
  toBuilder (payloadLength frame) `mappend`
  fromByteString (payload frame)

instance ToBuilder DataLength where
  toBuilder (DataLength dl) = fromWord24be dl

fromWord24be :: Word32 -> Builder
fromWord24be w =
  fromWord8 wHi8 `mappend`
  fromWord16be wLo16
    where wHi8 = fromIntegral $ (w `shiftR` 16) .&. 0xff
          wLo16 = fromIntegral $ w .&. 0xffff

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
    DataFrame d ->
      DataFrameHeader (streamID d)

toControlType :: ControlFrameDetails -> Word16
toControlType details =
  case details of
    SynStreamFrame _ -> cftSynStream
    SynReplyFrame _ -> cftSynReply
    RstStream _ _ -> cftRstStream
    Settings _ _ -> cftSettings
    Ping _ -> cftPing
    GoAway _ _ -> cftGoAway
    Headers _ _ _ -> cftHeaders
    WindowUpdate _ _ -> cftWindowUpdate
    Credential _ _ _ -> cftCredential

toFlagsByte :: Frame -> Word8
toFlagsByte frame =
  case frame of
    ControlFrame _ d ->
      case d of
        SynStreamFrame ss -> toWord8 $ synStreamFlags ss
        SynReplyFrame sr -> toWord8 $ synReplyFlags sr
        Settings f _ -> toWord8 f
        Headers f _ _ -> toWord8 f
        _ -> 0x0
    DataFrame d -> toWord8 $ dataFlags d


toPayload :: Deflate -> Frame -> IO ByteString
toPayload deflate frame =
  case frame of
    ControlFrame _ d ->
      toControlPayload deflate d
    DataFrame d ->
      return $ dataBytes d

toControlPayload :: Deflate -> ControlFrameDetails -> IO ByteString
toControlPayload deflate = fmap toByteString . toControlPayloadBuilder deflate

toControlPayloadBuilder :: Deflate -> ControlFrameDetails -> IO Builder
toControlPayloadBuilder deflate details =
  case details of
    SynStreamFrame ss ->
      fmap (toBuilder (synStreamNewStreamID ss) `mappend`
            toBuilder (synStreamAssociatedTo ss) `mappend`
            toBuilder (synStreamPriority ss) `mappend`
            toBuilder (synStreamSlot ss) `mappend`) $ compressHeaderBlock deflate $ synStreamHeaderBlock ss
    SynReplyFrame sr ->
      fmap (toBuilder (synReplyNewStreamID sr) `mappend`) $ compressHeaderBlock deflate $ synReplyHeaderBlock sr
    RstStream sid status ->
      return $ toBuilder sid `mappend` toBuilder status
    Settings _ pairs ->
      return $ toBuilder pairs
    Ping pid ->
      return $ toBuilder pid
    GoAway sid status ->
      return $ toBuilder sid `mappend` toBuilder status
    Headers _ sid hb ->
      return $ toBuilder sid `mappend` toBuilder hb
    WindowUpdate sid dws ->
      return $ toBuilder sid `mappend` toBuilder dws
    Credential slot proof certs ->
      return $ toBuilder slot `mappend` toBuilder proof `mappend` toBuilder certs

compressHeaderBlock :: Deflate -> HeaderBlock -> IO Builder
compressHeaderBlock deflate hb =
  compress deflate $ toByteString $ toBuilder hb


class ToBuilder a where
  toBuilder :: a -> Builder

instance ToBuilder StreamID where
  toBuilder (StreamID w) = fromWord32be w

instance ToBuilder (Maybe StreamID) where
  toBuilder = maybe (fromWord32be 0x0) toBuilder

instance ToBuilder Priority where
  toBuilder (Priority w) = fromWord8 (w `shiftL` 5)

instance ToBuilder Slot where
  toBuilder (Slot w) = fromWord8 w

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

instance ToBuilder TerminationStatus where
  toBuilder ts = fromWord32be $ case ts of
    ProtocolError -> tsProtocolError
    InvalidStream -> tsInvalidStream
    RefusedStream -> tsRefusedStream
    UnsupportedVersion -> tsUnsupportedVersion
    Cancel -> tsCancel
    InternalError -> tsInternalError
    FlowControlError -> tsFlowControlError
    StreamInUse -> tsStreamInUse
    StreamAlreadyClosed -> tsStreamAlreadyClosed
    InvalidCredentials -> tsInvalidCredentials
    FrameTooLarge -> tsFrameTooLarge
    TerminationStatusUnknown w -> w

instance ToBuilder [(SettingIDAndFlags, SettingValue)] where
  toBuilder pairs =
    fromWord32be (fromIntegral $ length pairs) `mappend`
    foldl' mappend mempty (map fromPair pairs)
      where fromPair (idfs, v) = toBuilder idfs `mappend` toBuilder v

instance ToBuilder SettingIDAndFlags where
  toBuilder stidfs =
    toBuilder (settingIDFlags stidfs) `mappend`
    toBuilder (settingID stidfs)

instance ToBuilder SettingID where
  toBuilder stid = fromWord24be $ case stid of
    SettingsUploadBandwidth -> stidSettingsUploadBandwidth
    SettingsDownloadBandwidth -> stidSettingsDownloadBandwidth
    SettingsRoundTripTime -> stidSettingsRoundTripTime
    SettingsMaxConcurrentStreams -> stidSettingsMaxConcurrentStreams
    SettingsCurrentCWND -> stidSettingsCurrentCWND
    SettingsDownloadRetransRate -> stidSettingsDownloadRetransRate
    SettingsInitialWindowSize -> stidSettingsInitialWindowSize
    SettingsClientCertificateVectorSize -> stidSettingsClientCertificateVectorSize
    SettingsOther w -> w

instance ToBuilder SettingValue where
  toBuilder (SettingValue sv) = fromWord32be sv

instance ToBuilder GoAwayStatus where
  toBuilder s = fromWord32be $ case s of
    GoAwayOK -> gsGoAwayOK
    GoAwayProtocolError -> gsGoAwayProtocolError
    GoAwayInternalError -> gsGoAwayInternalError
    GoAwayStatusUnknown w -> w

instance ToBuilder Slot16 where
  toBuilder (Slot16 w) = fromWord16be w

instance ToBuilder Proof where
  toBuilder (Proof bs) = lengthAndBytes bs

instance ToBuilder [Certificate] where
  toBuilder = foldl' mappend mempty . map toBuilder

instance ToBuilder Certificate where
  toBuilder (Certificate bs) = lengthAndBytes bs

instance ToBuilder (Flags f) where
  toBuilder (Flags w) = fromWord8 w

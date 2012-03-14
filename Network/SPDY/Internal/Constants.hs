{- | Constants used when serializing and deserializing SPDY frames. -}

module Network.SPDY.Internal.Constants where

import Data.Word (Word16, Word32)

cftSynStream :: Word16
cftSynStream = 0x1

cftSynReply :: Word16
cftSynReply = 0x2

cftRstStream :: Word16
cftRstStream = 0x3

cftSettings :: Word16
cftSettings = 0x4

cftPing :: Word16
cftPing = 0x6

cftGoAway :: Word16
cftGoAway = 0x7

cftHeaders :: Word16
cftHeaders = 0x8

cftWindowUpdate :: Word16
cftWindowUpdate = 0x9

cftCredential :: Word16
cftCredential = 0xB


tsProtocolError :: Word32
tsProtocolError = 1

tsInvalidStream :: Word32
tsInvalidStream = 2

tsRefusedStream :: Word32
tsRefusedStream = 3

tsUnsupportedVersion :: Word32
tsUnsupportedVersion = 4

tsCancel :: Word32
tsCancel = 5

tsInternalError :: Word32
tsInternalError =  6

tsFlowControlError :: Word32
tsFlowControlError = 7

tsStreamInUse :: Word32
tsStreamInUse = 8

tsStreamAlreadyClosed :: Word32
tsStreamAlreadyClosed = 9

tsInvalidCredentials :: Word32
tsInvalidCredentials = 10

tsFrameTooLarge :: Word32
tsFrameTooLarge = 11


stidSettingsUploadBandwidth :: Word32
stidSettingsUploadBandwidth = 1

stidSettingsDownloadBandwidth :: Word32
stidSettingsDownloadBandwidth = 2

stidSettingsRoundTripTime :: Word32
stidSettingsRoundTripTime = 3

stidSettingsMaxConcurrentStreams :: Word32
stidSettingsMaxConcurrentStreams = 4

stidSettingsCurrentCWND :: Word32
stidSettingsCurrentCWND = 5

stidSettingsDownloadRetransRate :: Word32
stidSettingsDownloadRetransRate = 6

stidSettingsInitialWindowSize :: Word32
stidSettingsInitialWindowSize = 7

stidSettingsClientCertificateVectorSize :: Word32
stidSettingsClientCertificateVectorSize = 8


gsGoAwayOK :: Word32
gsGoAwayOK = 0

gsGoAwayProtocolError :: Word32
gsGoAwayProtocolError = 1

gsGoAwayInternalError :: Word32
gsGoAwayInternalError = 11


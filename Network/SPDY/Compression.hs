module Network.SPDY.Compression (compressionDictionary,
                                 Inflate,
                                 decompress,
                                 Deflate,
                                 compress) where

import Blaze.ByteString.Builder
import Codec.Zlib (Inflate, withInflateInput, flushInflate,
                   Deflate, withDeflateInput, flushDeflate)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import Data.IORef (newIORef, modifyIORef, readIORef)
import Data.Monoid

-- | The compression dictionary to use for the zlib compression of headers.
compressionDictionary :: ByteString
compressionDictionary =
  BSC8.pack $
  "optionsgetheadpostputdeletetraceacceptaccept-charsetaccept-encodingaccept-" ++
  "languageauthorizationexpectfromhostif-modified-sinceif-matchif-none-matchi" ++
  "f-rangeif-unmodifiedsincemax-forwardsproxy-authorizationrangerefererteuser" ++
  "-agent10010120020120220320420520630030130230330430530630740040140240340440" ++
  "5406407408409410411412413414415416417500501502503504505accept-rangesageeta" ++
  "glocationproxy-authenticatepublicretry-afterservervarywarningwww-authentic" ++
  "ateallowcontent-basecontent-encodingcache-controlconnectiondatetrailertran" ++
  "sfer-encodingupgradeviawarningcontent-languagecontent-lengthcontent-locati" ++
  "oncontent-md5content-rangecontent-typeetagexpireslast-modifiedset-cookieMo" ++
  "ndayTuesdayWednesdayThursdayFridaySaturdaySundayJanFebMarAprMayJunJulAugSe" ++
  "pOctNovDecchunkedtext/htmlimage/pngimage/jpgimage/gifapplication/xmlapplic" ++
  "ation/xhtmltext/plainpublicmax-agecharset=iso-8859-1utf-8gzipdeflateHTTP/1" ++
  ".1statusversionurl" ++
  "\0"

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

-- TODO: this is almost identical to 'compress'. Factor out common code.
decompress :: Inflate -> ByteString -> IO Builder
decompress inflate bs = do
  bref <- newIORef mempty
  let popper = (maybe (return ()) addBS =<<)
      addBS dbs = modifyIORef bref (`mappend` fromByteString dbs)
  withInflateInput inflate bs popper
  flushInflate inflate
  readIORef bref

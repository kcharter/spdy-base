{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Concurrent (forkIO, killThread)
import qualified Control.Exception as CE
import Control.Monad (unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import Data.Char (toLower, isSpace)
import Network.Socket ()
import System.IO (withFile, hSetBinaryMode, IOMode(..), openBinaryFile, hClose, hIsEOF)
import Options

import Data.Certificate.KeyRSA (decodePrivate)
import Data.Certificate.X509 (X509, decodeCertificate)
import Data.PEM (pemParseLBS, pemContent)
import qualified Network.TLS as TLS

import Network.SPDY.Server

data ProgOptions =
  ProgOptions { optCertFile :: String,
                optKeyFile :: String,
                optPort :: Int,
                optStaticDir :: String,
                optNoTLS :: Bool }

instance Options ProgOptions where
  defineOptions =
    pure ProgOptions
    <*> simpleOption "cert-file" "server-cert.pem" "The certificate file to use for TLS. Default is 'server-cert.pem'."
    <*> simpleOption "key-file" "server-private-key.pem" ("The private key file to use for TLS. " ++
                                                          "Default is 'server-private-key.pem'.")
    <*> simpleOption "port" 16000 "The port on which to accept incoming connections. Default is 16000."
    <*> simpleOption "static-dir" "." "The static directory from which to serve files."
    <*> simpleOption "no-tls" False "Don't use TLS."

main :: IO ()
main = runCommand $ \opts _ -> do
  let port = fromIntegral $ optPort opts
      useTLS = not $ optNoTLS opts
      staticDir = optStaticDir opts
      sopts = defaultServerOptions { soptIncomingRequestHandler = requestHandler staticDir }
  theServer <- server sopts
  tid <- forkIO (if useTLS
                 then runTLSServer' port (optCertFile opts) (optKeyFile opts) theServer
                 else runSocketServer port theServer)
  quitLoop tid
  where runTLSServer' port certFile keyFile theServer = do
          cert <- getCertificate certFile
          key <- getKey keyFile
          runTLSServer port cert key theServer
        quitLoop tid = do
          s <- getLine
          let s' = map toLower $ dropWhile isSpace s
          case s' of
            'q':_ -> killThread tid
            _ ->
              quitLoop tid

-- TODO: why is 'maybePuller' unused here?
requestHandler :: FilePath -> RequestHandler
requestHandler staticDir (HeaderBlock pairs) maybePuller =
  maybe badRequest forPath $ lookup (HeaderName ":path") pairs
  where badRequest = return (responseHeaders 400, Nothing)
        forPath (HeaderValue pathBytes) =
          CE.catch (do h <- openBinaryFile (toFilePath pathBytes) ReadMode
                       return (responseHeaders 200, Just $ sendFile h))
          notFound
        toFilePath pathBytes = relativeTo staticDir (C8.unpack pathBytes)
        relativeTo staticDir path = staticDir ++ "/" ++ path
        sendFile h pushContent = CE.finally loop (hClose h)
          where loop = do chunk <- B.hGetSome h 1200
                          isLast <- hIsEOF h
                          pushContent $ moreData chunk isLast
                          unless isLast loop
        notFound :: CE.IOException -> IO (HeaderBlock, Maybe a)
        notFound _ = return (responseHeaders 404, Nothing)
        responseHeaders statusCode = HeaderBlock [httpStatus statusCode, http1_1Version]


getCertificate :: String -> IO X509
getCertificate certFileName =
  withFile certFileName ReadMode $ \h ->
    hSetBinaryMode h True >>
    LB.hGetContents h >>=
    either (error . ("decodeCertificate: " ++)) return .
    decodeCertificate .
    either (error . ("pemParseLBS: " ++)) (LB.fromChunks . map pemContent) .
    pemParseLBS

getKey :: String -> IO TLS.PrivateKey
getKey keyFileName =
  fmap TLS.PrivRSA $ withFile keyFileName ReadMode $ \h ->
  hSetBinaryMode h True >>
  LB.hGetContents h >>=
  either (error . ("decodePrivate: " ++)) (return . snd) .
  decodePrivate .
  either (error . ("pemParseLBS: " ++)) (LB.fromChunks . map pemContent) .
  pemParseLBS

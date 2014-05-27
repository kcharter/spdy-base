{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Concurrent (forkIO, killThread)
import Control.Monad (liftM, when)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Char (isSpace, toLower)
import Data.Time.LocalTime (getZonedTime)
import qualified Network.Socket as Sock
import System.IO

import Options

import qualified Crypto.Random as CR
import Data.Certificate.KeyRSA (decodePrivate)
import Data.Certificate.X509 (X509, decodeCertificate)
import Data.PEM (pemParseLBS, pemContent)
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSX

data ProgOptions =
  ProgOptions { optCertFile :: String,
                optKeyFile :: String,
                optPort :: Int }

instance Options ProgOptions where
  defineOptions =
    pure ProgOptions
    <*> simpleOption "cert-file" "server-cert.pem" "The certificate file for the server."
    <*> simpleOption "key-file" "server-private-key.pem" "The private key file for the server."
    <*> simpleOption "port" 15000 "The port number on which to accept incoming connections."

main :: IO ()
main = runCommand $ \opts _ -> do
  let port = fromIntegral $ optPort opts
  cert <- withFile (optCertFile opts) ReadMode $ \h ->
    hSetBinaryMode h True >>
    LB.hGetContents h >>=
    either (error . ("decodeCertificate: " ++)) return .
    decodeCertificate .
    either (error . ("pemParseLBS: " ++)) (LB.fromChunks . map pemContent) .
    pemParseLBS
  key <- fmap TLS.PrivRSA $ withFile (optKeyFile opts) ReadMode $ \h ->
    hSetBinaryMode h True >>
    LB.hGetContents h >>=
    either (error . ("decodePrivate: " ++)) (return . snd) .
    decodePrivate .
    either (error . ("pemParseLBS: " ++)) (LB.fromChunks . map pemContent) .
    pemParseLBS
  tid <- forkIO (runServer port cert key)
  quitLoop tid
  where quitLoop tid = do
          s <- getLine
          let s' = map toLower $ dropWhile isSpace s
          case s' of
            'q':_ -> killThread tid
            _ ->
              quitLoop tid


runServer :: Sock.PortNumber -> X509 -> TLS.PrivateKey -> IO ()
runServer port cert key = do
  putStrLn $ "running server on port " ++ show port
  putStrLn $ "press 'q' to quit."
  let hints = Sock.defaultHints { Sock.addrFlags = [ Sock.AI_ADDRCONFIG ] }
  let ip4SockAddresses  =
        filter isIP4SockAddress .
        map Sock.addrAddress
      isIP4SockAddress addr = case addr of
        (Sock.SockAddrInet _ _) -> True
        _ -> False
  addrs <- liftM ip4SockAddresses $
           Sock.getAddrInfo (Just hints) (Just "localhost") Nothing
  when (null addrs) (error "Cannot find a socket address to bind to.")
  let (Sock.SockAddrInet _ hn) = head addrs
  s <- Sock.socket Sock.AF_INET Sock.Stream Sock.defaultProtocol
  Sock.bindSocket s (Sock.SockAddrInet port hn)
  Sock.listen s 2
  rng <- (liftM CR.cprgCreate CR.createEntropyPool :: IO CR.SystemRNG)
  let tlsParams =
        TLS.defaultParamsServer { TLS.pCiphers = TLSX.ciphersuite_all
                                , TLS.pCertificates = [(cert, Just key)]
                                , TLS.onSuggestNextProtocols =
                                  return (Just ["hello", "time"]) }
  loop tlsParams rng s
  where loop tlsParams rng s = do
          (s', sa) <- Sock.accept s
          putStrLn $ "accepted connection from " ++ show sa
          forkIO (Sock.socketToHandle s' ReadWriteMode >>= \h -> do
                     tlsCtx <- TLS.contextNewOnHandle h tlsParams rng
                     TLS.handshake tlsCtx
                     maybeProtocol <- TLS.getNegotiatedProtocol tlsCtx
                     maybe (TLS.bye tlsCtx) (handleProtocol tlsCtx) maybeProtocol
                     TLS.sendData tlsCtx "hello!\n")
          loop tlsParams rng s
        handleProtocol tlsCtx protocol =
          case C8.unpack protocol of
            "hello" -> TLS.sendData tlsCtx "hello!\n"
            "time"  -> do time <- getZonedTime
                          TLS.sendData tlsCtx (LC8.pack $ show time)
            huh     -> TLS.sendData tlsCtx $ LC8.pack $ "unknown protocol: " ++ huh

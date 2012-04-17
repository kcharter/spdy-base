{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TlsServer where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Char (isSpace, toLower)
import Data.Maybe (listToMaybe)
import Data.Time.LocalTime (getZonedTime)
import Network.Socket
import System.Environment (getArgs)
import System.IO

import Options

import qualified Crypto.Random as CR
import Data.Certificate.PEM (parsePEMCert, parsePEMKeyRSA)
import Data.Certificate.KeyRSA (decodePrivate)
import Data.Certificate.X509 (X509, decodeCertificate)
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSX

defineOptions "Opts" $ do
  stringOption "optCertFile" "cert-file" "server-cert.pem" "The certificate file for the server."
  stringOption "optKeyFile" "key-file" "server-private-key.pem" "The private key file for the server."
  intOption "optPort" "port" 15000 "The port number on which to accept incoming connections."

main :: IO ()
main = runCommand $ \opts _ -> do
  let port = fromIntegral $ optPort opts
  cert <- withFile (optCertFile opts) ReadMode $ \h ->
    hSetBinaryMode h True >>
    B.hGetContents h >>=
    either error return .
    decodeCertificate .
    maybe (error "Can't parse PEM certificate") (LB.fromChunks . (:[])) .
    parsePEMCert
  key <- fmap TLS.PrivRSA $ withFile (optKeyFile opts) ReadMode $ \h ->
    hSetBinaryMode h True >>
    B.hGetContents h >>=
    either error (return . snd) .
    decodePrivate .
    maybe (error "Can't parse PEM key") (LB.fromChunks . (:[])) .
    parsePEMKeyRSA
  tid <- forkIO (runServer port cert key)
  quitLoop tid
  where quitLoop tid = do
          s <- getLine
          let s' = map toLower $ dropWhile isSpace s
          case s' of
            'q':_ -> killThread tid
            _ ->
              quitLoop tid


runServer :: PortNumber -> X509 -> TLS.PrivateKey -> IO ()
runServer port cert key = do
  putStrLn $ "running server on port " ++ show port
  putStrLn $ "press 'q' to quit."
  let hints = defaultHints { addrFlags = [ AI_ADDRCONFIG ] }
  addrs <- getAddrInfo (Just hints) (Just "localhost") Nothing
  when (null addrs) (error "Cannot find a socket address to bind to.")
  let (SockAddrInet _ hn) = addrAddress $ head addrs
  s <- socket AF_INET Stream defaultProtocol
  bindSocket s (SockAddrInet port hn)
  listen s 2
  rng <- (CR.newGenIO :: IO CR.SystemRandom)
  let tlsParams = TLS.defaultParams { TLS.pCiphers = TLSX.ciphersuite_all
                                    , TLS.pCertificates = [(cert, Just key)]
                                    , TLS.onSuggestNextProtocols =
                                      return (Just ["hello", "time"]) }
  loop tlsParams rng s
  where loop tlsParams rng s = do
          (s', sa) <- accept s
          putStrLn $ "accepted connection from " ++ show sa
          forkIO (socketToHandle s' ReadWriteMode >>= \h -> do
                     tlsCtx <- TLS.server tlsParams rng h
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

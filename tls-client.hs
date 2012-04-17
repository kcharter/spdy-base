{-# LANGUAGE TemplateHaskell #-}

module TlsClient where

import qualified Data.ByteString.Char8 as C8
import Network
import System.Environment
import System.IO

import Options

import qualified Crypto.Random as CR
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSX

defineOptions "Opts" $ do
  stringOption "optServer" "server" "localhost" "Server name or IP address."
  intOption "optPort" "port" 15000 "Server port to connect to."
  stringOption "optProtocol" "protocol" "hello" "Protocol to negotiate with the server."

main :: IO ()
main = runCommand $ \opts _ -> do
  let hostName = optServer opts
      port = fromIntegral $ optPort opts
      proto = C8.pack $ optProtocol opts
      tlsParams = TLS.defaultParams { TLS.pCiphers = TLSX.ciphersuite_all
                                    , TLS.onNPNServerSuggest = Just $ const (return proto) }
  rng <- CR.newGenIO :: IO CR.SystemRandom
  h <- connectTo hostName (PortNumber port)
  log "connected"
  tlsCtx <- TLS.client tlsParams rng h
  log "created TLS context"
  TLS.handshake tlsCtx
  log "made TLS handshake"
  s <- TLS.recvData tlsCtx
  log "received data"
  TLS.bye tlsCtx
  log "closed TLS session"
  hClose h
  log "closed socket"
  C8.putStrLn s
  where log msg = putStrLn msg >> hFlush stdout

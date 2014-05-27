module Main where

import Control.Applicative
import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as C8
import Network
import System.IO

import Options

import qualified Crypto.Random as CR
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSX

data ProgOptions =
  ProgOptions { optServer :: String,
                optPort :: Int,
                optProtocol :: String }

instance Options ProgOptions where
  defineOptions =
    pure ProgOptions
    <*> simpleOption "server" "localhost" "Server name or IP address."
    <*> simpleOption "port" 15000 "Server port to connect to."
    <*> simpleOption "protocol" "hello" "Protocol to negotiate with the server."

main :: IO ()
main = runCommand $ \opts _ -> do
  let hostName = optServer opts
      port = fromIntegral $ optPort opts
      proto = C8.pack $ optProtocol opts
      tlsParams =
        TLS.defaultParamsClient { TLS.pCiphers = TLSX.ciphersuite_all
                                , TLS.onNPNServerSuggest = Just $ const (return proto) }
  rng <- (liftM CR.cprgCreate CR.createEntropyPool :: IO CR.SystemRNG)
  h <- connectTo hostName (PortNumber port)
  log "connected"
  tlsCtx <- TLS.contextNewOnHandle h tlsParams rng
  log "created TLS context"
  TLS.handshake tlsCtx
  log "made TLS handshake"
  maybeProto <- TLS.getNegotiatedProtocol tlsCtx
  log $ "the negotiated protocol is " ++ show maybeProto
  s <- TLS.recvData tlsCtx
  log "received data"
  TLS.bye tlsCtx
  log "closed TLS session"
  hClose h
  log "closed socket"
  C8.putStrLn s
  where log msg = putStrLn msg >> hFlush stdout

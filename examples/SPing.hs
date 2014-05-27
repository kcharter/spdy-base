{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad (replicateM_)
import System.IO (hSetBuffering, BufferMode(..), stdout, stderr)

import Options

import Network.SPDY.Client

data ProgOptions =
  ProgOptions { optHost :: String,
                optPort :: Int,
                optIters :: Int,
                optNoTLS :: Bool }

instance Options ProgOptions where
  defineOptions =
    pure ProgOptions
    <*> simpleOption "host" "localhost" "The host name or IP address of the server."
    <*> simpleOption "port" 10041 "The server port."
    <*> simpleOption "iters" 10 "The number of pings to send."
    <*> simpleOption "no-tls" False "Don't use TLS, just an ordinary unencrypted network connection."

main :: IO ()
main = runCommand $ \opts _ -> do
  let host = optHost opts
      port = fromIntegral $ optPort opts
      useTLS = not $ optNoTLS opts
      cKey = OriginKey (Origin (if useTLS then "https" else "http") (Host host) port)
  c <- client $ defaultClientOptions {
    coptConnectionStyle = if useTLS then CsTLS else CsSocket }
  let bufferMode = BlockBuffering (Just 4096)
  hSetBuffering stdout bufferMode
  hSetBuffering stderr bufferMode
  putStrLn $ "Pinging SPDY server at " ++ host ++ ":" ++ show port
  replicateM_ (optIters opts) (doPing c cKey)
  where doPing c cKey =
          ping defaultPingOptions c cKey >>=
          (putStrLn . \pr -> case pr of
              PingResponse t -> "round-trip time: " ++ show t
              PingTimeout  t -> "timed out after " ++ show t)


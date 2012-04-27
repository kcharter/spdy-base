{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SPing where

import Control.Monad (replicateM_)
import Network (PortNumber)
import System.Environment (getArgs)
import System.IO (hSetBuffering, BufferMode(..), stdout, stderr)

import Options

import Network.SPDY.Client

defineOptions "Opts" $ do
  stringOption "optHost" "host" "localhost" "The host name or IP address of the server."
  intOption "optPort" "port" 10041 "The server port."
  intOption "optIters" "iters" 10 "The number of pings to send."
  boolOption "optNoTLS" "no-tls" False "Don't use TLS, just an ordinary unencrypted network connection."

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


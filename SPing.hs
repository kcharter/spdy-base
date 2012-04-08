{-# LANGUAGE OverloadedStrings #-}

module SPing where

import Control.Monad (replicateM_)
import Network (PortNumber)
import System.Environment (getArgs)
import System.IO (hSetBuffering, BufferMode(..), stdout, stderr)

import Network.SPDY.Client

main :: IO ()
main = do
  opts <- getOptions
  c <- client defaultClientOptions
  let host = optsHost opts
      port = optsPort opts
      cKey = OriginKey (Origin "https" (Host host) port)
  let bufferMode = BlockBuffering (Just 4096)
  hSetBuffering stdout bufferMode
  hSetBuffering stderr bufferMode
  putStrLn $ "Pinging SPDY server at " ++ host ++ ":" ++ show port
  replicateM_ (optsIters opts) (doPing c cKey)
  where doPing c cKey =
          ping defaultPingOptions c cKey >>=
          (putStrLn . \pr -> case pr of
              PingResponse t -> "round-trip time: " ++ show t
              PingTimeout  t -> "timed out after " ++ show t)

getOptions :: IO Options
getOptions =
  getHost defaultOptions `fmap` getArgs
    where getHost opts [] = opts
          getHost opts (h:r) = getPort (opts { optsHost = h }) r
          getPort opts [] = opts
          getPort opts (p:r) = getIters (opts { optsPort = fromInteger $ read p }) r
          getIters opts [] = opts
          getIters opts (n:_) = opts { optsIters = read n }

data Options = Options {
  optsHost :: String,
  optsPort :: PortNumber,
  optsIters :: Int
  }

defaultOptions = Options {
  optsHost = "127.0.0.1",
  optsPort = fromInteger 10041,
  optsIters = 10
  }
{-# LANGUAGE OverloadedStrings #-}

module SGet where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString as B
import Data.String (fromString)
import Network (PortNumber)
import System.Environment (getArgs)
import System.IO (hFlush, stdout, stderr, BufferMode(..), hSetBuffering)

import Network.SPDY.Client
import Network.SPDY.Frames (HeaderName(..), HeaderValue(..))

main :: IO ()
main = do
  opts <- getOptions
  c <- client defaultClientOptions
  let host = optsHost opts
      port = optsPort opts
      file = optsFileName opts
  putStrLn $ "Downloading '" ++ file ++ "' from SPDY server at " ++ host ++ ":" ++ show port
  -- We're using stdout for output and stderr for frame logging, so we
  -- want to make sure that we're using block buffering on both. This
  -- makes a huge difference.
  let bufferMode = BlockBuffering (Just 4096)
  hSetBuffering stdout bufferMode
  hSetBuffering stderr bufferMode
  doDownload c host port file
  where doDownload c host port file = do
          doneHeaders <- newEmptyMVar
          doneData <- newEmptyMVar
          let cKey = OriginKey (Origin "https" (Host host) port)
              -- Apparently, the flip server does not use the
              -- prescribed special header names that start with a
              -- colon. Instead, it expects the headers 'method',
              -- 'url' and 'scheme' in the request. It can also crash
              -- with a segmentation fault if only the 'method' and
              -- 'url' headers are present.
              headers = [(HeaderName ":method", HeaderValue "GET"),
                         (HeaderName ":path", HeaderValue $ fromString $ "/" ++ file),
                         (HeaderName ":version", HeaderValue "HTTP/1.1"),
                         (HeaderName ":host", HeaderValue $ fromString $ host ++ ":" ++ show port),
                         (HeaderName ":scheme", HeaderValue "http"),
                         (HeaderName "method", HeaderValue "GET"),
                         (HeaderName "url", HeaderValue $ fromString $ "http://" ++ host ++ ":" ++ show port ++ "/" ++ file),
                         (HeaderName "scheme", HeaderValue "http")]
              consumeHeaders Nothing =
                putMVar doneHeaders ()
              consumeHeaders (Just headers) =
                mapM_ printHeader headers >> putStr "\r\n"
                where printHeader (HeaderName name, HeaderValue value) =
                        B.putStr name >> putStr " = " >> B.putStr value >> putStr "\r\n"
              consumeData Nothing =
                putMVar doneData ()
              consumeData (Just bytes) =
                B.putStr bytes
          sid <- initiateStream c cKey Nothing headers (return Nothing) consumeHeaders consumeData
          putStrLn $ "Initiated stream " ++ show sid
          hFlush stdout
          takeMVar doneHeaders
          takeMVar doneData

getOptions :: IO Options
getOptions =
  getHost defaultOptions `fmap` getArgs
    where getHost opts [] = opts
          getHost opts (h:r) = getPort (opts { optsHost = h }) r
          getPort opts [] = opts
          getPort opts (p:r) = getFileName (opts { optsPort = fromInteger $ read p }) r
          getFileName opts [] = opts
          getFileName opts (fn:_) = opts { optsFileName = fn }

data Options = Options {
  optsHost :: String,
  optsPort :: PortNumber,
  optsFileName :: String
  }

defaultOptions = Options {
  optsHost = "127.0.0.1",
  optsPort = fromInteger 10041,
  optsFileName = "hello.html"
  }

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SGet where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import qualified Data.ByteString as B
import Data.String (fromString)
import Network (PortNumber)
import System.Environment (getArgs)
import System.IO (hFlush, stdout, stderr, BufferMode(..), hSetBuffering)

import Options

import Network.SPDY.Client
import Network.SPDY.Frames (HeaderName(..), HeaderValue(..))

defineOptions "Opts" $ do
  stringOption "optHost" "host" "localhost" "The host name or IP address of the server."
  intOption "optPort" "port" 10041 "The server port."
  stringOption "optPath" "path" "/hello.html" "The path of the resource to get."
  boolOption "optNoTLS" "no-tls" False "Don't use TLS, just an ordinary unencrypted network connection."

main :: IO ()
main = runCommand $ \opts _ -> do
  let host = optHost opts
      port = fromIntegral $ optPort opts
      path = optPath opts
      useTLS = not $ optNoTLS opts
  c <- client defaultClientOptions {
    coptConnectionStyle = if useTLS then CsTLS else CsSocket }
  putStrLn $ "Downloading '" ++ path ++ "' from SPDY server at " ++ host ++ ":" ++ show port
  -- We're using stdout for output and stderr for frame logging, so we
  -- want to make sure that we're using block buffering on both. This
  -- makes a huge difference.
  let bufferMode = BlockBuffering (Just 4096)
  hSetBuffering stdout bufferMode
  hSetBuffering stderr bufferMode
  doDownload c host port useTLS path
  where doDownload c host port useTLS path = do
          doneHeaders <- newEmptyMVar
          doneData <- newEmptyMVar
          let cKey = OriginKey (Origin (if useTLS then "https" else "http") (Host host) port)
              -- Apparently, the flip server does not use the
              -- prescribed special header names that start with a
              -- colon. Instead, it expects the headers 'method',
              -- 'url' and 'scheme' in the request. It can also crash
              -- with a segmentation fault if only the 'method' and
              -- 'url' headers are present.
              headers = [(HeaderName ":method", HeaderValue "GET"),
                         (HeaderName ":path", HeaderValue $ fromString $ "/" ++ path),
                         (HeaderName ":version", HeaderValue "HTTP/1.1"),
                         (HeaderName ":host", HeaderValue $ fromString $ host ++ ":" ++ show port),
                         (HeaderName ":scheme", HeaderValue "http"),
                         (HeaderName "method", HeaderValue "GET"),
                         (HeaderName "url", HeaderValue $ fromString $ "http://" ++ host ++ ":" ++ show port ++ "/" ++ path),
                         (HeaderName "scheme", HeaderValue "http")]
              opts = defaultStreamOptions {
                streamOptsHeaderConsumer = \maybeHeaders ->
                 case maybeHeaders of
                   Nothing ->
                     putMVar doneHeaders ()
                   Just headers ->
                     mapM_ printHeader headers >> putStr "\r\n",
                streamOptsDataConsumer = \maybeBytes ->
                  case maybeBytes of
                    Nothing ->
                      putMVar doneData () >> return 0
                    Just bytes ->
                      B.putStr bytes >> return (fromIntegral $ B.length bytes)
                }
              printHeader (HeaderName name, HeaderValue value) =
                B.putStr name >> putStr " = " >> B.putStr value >> putStr "\r\n"
          sid <- initiateStream c cKey headers opts
          putStrLn $ "Initiated stream " ++ show sid
          hFlush stdout
          takeMVar doneHeaders
          takeMVar doneData

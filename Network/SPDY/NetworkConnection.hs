-- | A representation for low-level data connections. This allows
-- using TLS optionally, or building simulated network connections for
-- unit tests where the two endpoints are in the same process.

module Network.SPDY.NetworkConnection where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import System.IO (Handle, hFlush, hClose)

import Network.TLS (TLSCtx)
import qualified Network.TLS as TLS


-- | A colleciton of fundamental IO actions common to low-level data
-- connections.
data NetworkConnection =
  NetworkConnection
  { sendData :: ByteString -> IO ()
    -- ^ Send the contents of a strict byte string to the remote endpoint.
  , receiveData :: IO ByteString
    -- ^ Receive a strict byte string from the remote endpoint,
    -- blocking if no data is available.
  , flush :: IO ()
    -- ^ Flush any buffered data, forcing a send to the remote endpoint.
  , close :: IO ()
    -- ^ Shut down the connection.
  }


-- | A data connection based on a handle open for read-write
-- access. This is primarily intended for working with socket handles
-- without using TLS.
fromHandle :: Handle -> NetworkConnection
fromHandle h =
  NetworkConnection
  { sendData = B.hPut h
  , receiveData = B.hGetSome h 4096
  , flush = hFlush h
  , close = hClose h }

-- | A data connection based on a TLS context on a handle. This is
-- intended for TLS-enabled connections over sockets.
fromTLSCtx :: TLSCtx Handle -> NetworkConnection
fromTLSCtx tlsCtx =
  NetworkConnection
  { sendData = TLS.sendData tlsCtx . LB.fromChunks . (:[])
  , receiveData = TLS.recvData tlsCtx
  , flush = hFlush (TLS.ctxConnection tlsCtx)
  , close = TLS.bye tlsCtx >> hClose (TLS.ctxConnection tlsCtx) }

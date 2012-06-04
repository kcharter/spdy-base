{-# LANGUAGE OverloadedStrings #-}

module Network.SPDY.Server
       ( -- * Creating servers
         ServerOptions(..),
         defaultServerOptions,
         http1_1NotFound,
         Server,
         server,
         -- * Accepting incoming connections
         acceptConnection,
         -- * Standard server loops
         runSocketServer,
         runTLSServer
       ) where

import Control.Concurrent (forkIO)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as C8
import Network.Socket (PortNumber)
import qualified Network.Socket as Sock
import System.IO (IOMode(ReadWriteMode), hPutStrLn, stderr)

import qualified Crypto.Random as CR
import Data.Certificate.X509 (X509)
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSX

import Network.SPDY.Endpoint
import Network.SPDY.Frames
import Network.SPDY.NetworkConnection (NetworkConnection, fromHandle, fromTLSCtx)

data ServerOptions =
  ServerOptions { soptConnectionTimeoutSeconds :: Maybe Int,
                  -- ^ The number of seconds without any activity
                  -- before closing a connection. If 'Nothing', there
                  -- is no timeout.
                  soptIncomingRequestHandler :: RequestHandler
                  -- ^ The handler for incoming requests.
                  }

-- | Default options with no connection timeouts and a request handler
-- that always responds with an HTTP 1.1 404 error.
defaultServerOptions :: ServerOptions
defaultServerOptions =
  ServerOptions { soptConnectionTimeoutSeconds = Nothing,
                  soptIncomingRequestHandler = http1_1NotFound }

-- | Responds to all requests with an HTTP 1.1 404 response. This is a
-- reasonable default for implementing HTTP-over-SPDY servers.
http1_1NotFound :: RequestHandler
http1_1NotFound _ _ =
  return (HeaderBlock [ (HeaderName ":status", HeaderValue "404 Not found"),
                        (HeaderName ":version", HeaderValue "HTTP/1.1") ],
          Nothing)

-- | A SPDY server, and endpoint which accepts incoming network
-- connections and serves content requests.
data Server =
  Server {
    serverEndpoint :: Endpoint
    -- ^ The underlying SPDY endpoint for the server.
    }

-- | Creates a SPDY server with the given options.
server :: ServerOptions -> IO Server
-- TODO respect the connection timeout in the server options
server options = do
  ep <- endpoint $ EndpointOptions {
    epOptsFirstPingID = PingID 0,
    epOptsFirstStreamID = StreamID 2,
    epOptsIncomingRequestHandler = soptIncomingRequestHandler options,
    epOptsInputFrameHandlers = defaultEndpointInputFrameHandlers
    }
  return $ Server { serverEndpoint = ep }

-- | Establishes a new SPDY connection in a server for a given
-- low-level network connection. With this function, you can roll your
-- own server loop.
acceptConnection :: Server -> ConnectionKey -> NetworkConnection -> IO ()
acceptConnection s = addConnection (serverEndpoint s)

-- | Opens a server socket on a given port, and submits incoming
-- connections to the given SPDY server.
runSocketServer :: PortNumber
                   -- ^ The port on which to accept connections.
                   -> Server
                   -- ^ The SPDY server.
                   -> IO ()
runSocketServer pn server = do
  let hints = Sock.defaultHints { Sock.addrFlags = [ Sock.AI_ADDRCONFIG ] }
  addrs <- Sock.getAddrInfo (Just hints) (Just "localhost") Nothing
  when (null addrs) (error "Cannot find a socket address to bind to.")
  let (Sock.SockAddrInet _ hn) = Sock.addrAddress $ head addrs
  s <- Sock.socket Sock.AF_INET Sock.Stream Sock.defaultProtocol
  Sock.bindSocket s (Sock.SockAddrInet pn hn)
  Sock.listen s 2
  loop s
  where loop s = do
          (s', sa) <- Sock.accept s
          -- TODO: proper logging
          hPutStrLn stderr $ "accepted connection from " ++ show sa
          forkIO (Sock.socketToHandle s' ReadWriteMode >>=
                     (acceptConnection server (fromSockAddr sa) . fromHandle))
          loop s

-- | Opens a server socket on a given port, and submits TLS-protected
-- network connections to a SPDY server.
runTLSServer :: PortNumber
                -- ^ The port on which to accept connections.
                -> X509
                -- ^ The server's X509 certificate.
                -> TLS.PrivateKey
                -- ^ The server's private key.
                -> Server
                -- ^ The SPDY server.
                -> IO ()
runTLSServer pn cert key theServer = do
  let hints = Sock.defaultHints { Sock.addrFlags = [ Sock.AI_ADDRCONFIG ] }
  addrs <- Sock.getAddrInfo (Just hints) (Just "localhost") Nothing
  when (null addrs) (error "Cannot find a socket address to bind to.")
  let (Sock.SockAddrInet _ hn) = Sock.addrAddress $ head addrs
  s <- Sock.socket Sock.AF_INET Sock.Stream Sock.defaultProtocol
  Sock.bindSocket s (Sock.SockAddrInet pn hn)
  Sock.listen s 2
  rng <- (CR.newGenIO :: IO CR.SystemRandom)
  let tlsParams = TLS.defaultParams { TLS.pCiphers = TLSX.ciphersuite_all
                                    , TLS.pCertificates = [(cert, Just key)]
                                    , TLS.onSuggestNextProtocols =
                                      return (Just ["spdy/3"]) }
  loop tlsParams rng s
  where loop tlsParams rng s = do
          (s', sa) <- Sock.accept s
          -- TODO: proper logging
          hPutStrLn stderr $ "accepted connection from " ++ show sa
          forkIO (Sock.socketToHandle s' ReadWriteMode >>= \h -> do
                     tlsCtx <- TLS.server tlsParams rng h
                     TLS.handshake tlsCtx
                     maybeProtocol <- TLS.getNegotiatedProtocol tlsCtx
                     maybe (TLS.bye tlsCtx) (acceptSPDY sa tlsCtx) maybeProtocol)
          loop tlsParams rng s
        acceptSPDY sa tlsCtx protocol =
          case C8.unpack protocol of
            "spdy/3" ->
              acceptConnection theServer (fromSockAddr sa) (fromTLSCtx tlsCtx)
            _ ->
              TLS.bye tlsCtx

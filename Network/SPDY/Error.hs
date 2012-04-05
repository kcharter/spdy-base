{-# LANGUAGE DeriveDataTypeable #-}

-- | SPDY errors, represented as exceptions.
module Network.SPDY.Error where

import Control.Exception (Exception, SomeException)
import Data.Typeable (Typeable)

-- | SPDY errors.
data SPDYException =
  ConnectionCorrupted (Maybe SomeException) |
  -- ^ A connection has experienced an error that may have corrupted
  -- its header compression and decompression contexts. Typically,
  -- this will be caused by an IO error or a zlib error of some
  -- kind. When throwing this kind of exception, the cause, if any,
  -- should be included.
  OutOfStreamIDs
  -- ^ An endpoint has run out of stream IDs for initiating new
  -- streams. Unlike ping IDs, the SPDY spec does not permit reuse of
  -- stream IDs on a connection. Once a connection runs out of stream
  -- IDs, it should throw this error, and cleanly shut itself down.
  deriving (Typeable, Show)

instance Exception SPDYException

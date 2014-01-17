{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Utilities for syntactic processing of URLs.

module Network.SPDY.Url where

import Data.String (IsString)
import Network (HostName)
import Network.Socket (PortNumber)

-- | A web origin, as defined in RFC 6454.
data Origin = Origin { originScheme :: Scheme
                     , originHost :: Host
                     , originPort :: PortNumber } deriving (Eq, Ord, Show)

-- | The scheme part of a URI.
newtype Scheme = Scheme String deriving (Eq, Ord, Show, IsString)

-- | The host part of a URI.
newtype Host = Host String deriving (Eq, Ord, Show, IsString)

toHostName :: Host -> HostName
toHostName (Host host) = host



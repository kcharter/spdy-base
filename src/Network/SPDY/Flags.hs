{- | Tools for working with flags.

Each SPDY frame has a byte of flags in the same location. However, the
set of valid flags and the meanings of the flag bits varies depending
on the type of frame. For example, 0x1 can mean different things in
different types of frames.

This module provides a parametric 'Flags' type as an interface to a
raw one-byte bit set, where the type parameter is a phantom type
representing the kind of flags in the collection. When the phantom
type is in the 'Flag' type class, we can test and set just the allowed
flags in the collection.  The idea is to make it impossible to use the
flags for one kind of frame with a different kind of frame. -}

module Network.SPDY.Flags where

import Data.Bits (Bits, testBit, setBit)
import Data.Word (Word8)

import Network.SPDY.Internal.ToWord8

-- | Types that represent flag values. Each unique value has its own
-- bit position. Positions are limited to the range @0@ to @7@.
class Flag f where
  bit :: f -> Int
  -- ^ The bit position for a flag value.

-- | A collection of up to eight flags, associated with a particular flag type.
newtype Flags f = Flags Word8 deriving (Eq, Show, Read)

instance ToWord8 (Flags f) where
  toWord8 (Flags w) = w

-- | The collection in which no flag is set.
allClear :: Flags f
allClear = Flags 0

-- | Tests whether a flag is set in a collection.
isSet :: Flag f => f -> Flags f -> Bool
isSet f (Flags w) = testBit w (bit f)

-- | Sets a flag on a collection.
set :: Flag f => f -> Flags f -> Flags f
set f (Flags w) = Flags $ setBit w (bit f)

-- | Assembles a collection from the list of flags that should be set.
packFlags :: Flag f => [f] -> Flags f
packFlags = foldr set allClear

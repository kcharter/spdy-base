{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{- | Tools for working with flags.

Each SPDY frame has a byte of flags in the same location. However, the
set of valid flags and the meanings of the flag bits varies depending
on the type of frame. For example, 0x1 can mean different things in
different types of frames.

The classes in this module allow pairing unique flag types with
corresponding flag collection types. Generally, a flag type will be an
enumeration of some kind, and the collection type will be a @newtype@
wrapper around 'Word8'. The idea is to make it impossible to use the
flags for one kind of frame with a different kind of frame. -}

module Network.SPDY.Flags where

import Data.Bits (Bits, testBit, setBit)
import Data.Word (Word8)

-- | A collection of flags, encoded as bits.
class Bits fs => Flags fs where
  allClear :: fs
  -- ^ The collection in which no flag is set.

instance Flags Word8 where
  allClear = 0

-- | Relates a type of individual flags, generally an enumeration, to
-- a corresponding type of flag collection. The minimum implementation
-- is just 'bit'.
class Flags fs => Flag f fs | f -> fs, fs -> f where
  bit :: f -> Int
  -- ^ The bit position for a flag value.
  isSet :: f -> fs -> Bool
  -- ^ Tests whether a flag is set in a collection.
  isSet f fs = testBit fs (bit f)
  set :: f -> fs -> fs
  -- ^ Sets a flag on a collection.
  set f fs = setBit fs (bit f)
  packFlags :: [f] -> fs
  -- ^ Assembles a collection from the list of flags that should be
  -- set.
  packFlags = foldr set allClear

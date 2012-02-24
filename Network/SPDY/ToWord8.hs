module Network.SPDY.ToWord8 where

import Data.Word (Word8)

class ToWord8 a where
  toWord8 :: a -> Word8

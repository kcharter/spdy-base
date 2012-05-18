{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BoundedBufferTests where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Exception (assert, catch, ErrorCall(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Prelude hiding (catch)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Property (morallyDubiousIOProperty)

import Network.SPDY.Internal.BoundedBuffer (Sized(..))
import qualified Network.SPDY.Internal.BoundedBuffer as BB

test :: Test
test = testGroup "Bounded buffer tests" [
  testProperty "capacity" $ forAll withSpaceFor prop_capacity,
  testProperty "fifo" $ forAll withSpaceForEach prop_fifo,
  testProperty "constant total capacity" $ forAll withSpaceForAll prop_constTotalCapacity,
  testProperty "error on impossible add" $ forAll withoutSpaceFor prop_oversizedError
  ]

prop_capacity = morallyDubiousIOProperty . propIO_capacity

propIO_capacity :: (Content, Int) -> IO Bool
propIO_capacity (content, capacity) =
  assert (size content <= capacity) $ do
    bb <- BB.new capacity
    BB.add bb content
    rc <- BB.remainingCapacity bb
    return (rc == capacity - size content)

prop_fifo = morallyDubiousIOProperty . propIO_fifo

propIO_fifo :: ([Content], Int) -> IO Bool
propIO_fifo (contents, capacity) =
  assert (all (capacity >=) (map size contents)) $ do
    bb <- BB.new capacity
    forkIO (mapM_ (BB.add bb) contents)
    contents' <- mapM (const (BB.remove bb)) contents
    return (contents' == contents)

prop_constTotalCapacity = morallyDubiousIOProperty . propIO_constTotalCapacity

propIO_constTotalCapacity :: ([Content], Int) -> IO Bool
propIO_constTotalCapacity (contents, capacity) =
  assert (size contents <= capacity) $ do
    bb <- BB.new capacity
    totalCapacities <- sequence $
                       return (BB.totalCapacity bb):     -- initial
                       map (add' bb) contents ++         -- after each addition
                       map (const $ remove' bb) contents -- after each removal
    return (all (capacity ==) totalCapacities)
      where add' bb c = BB.add bb c >> return (BB.totalCapacity bb)
            remove' bb = BB.remove bb >> return (BB.totalCapacity bb)

prop_oversizedError = morallyDubiousIOProperty . propIO_oversizedError

propIO_oversizedError :: (Content, Int) -> IO Bool
propIO_oversizedError (content, capacity) =
  assert (size content > capacity) $ do
    bb <- BB.new capacity
    (BB.add bb content >> return False) `catch` handleErrorCall
    where handleErrorCall (ErrorCall _) = return True

withSpaceFor :: Gen (Content, Int)
withSpaceFor = do
  content <- arbitrary
  capacity <- thatCanHold content
  return (content, capacity)

withoutSpaceFor :: Gen (Content, Int)
withoutSpaceFor = do
  content <- arbitrary `suchThat` ((0 <) . size)
  capacity <- choose (0, size content - 1)
  return (content, capacity)

withSpaceForAll :: Gen ([Content], Int)
withSpaceForAll = do
  contents <- arbitrary
  capacity <- thatCanHold contents
  return (contents, capacity)

withSpaceForEach :: Gen ([Content], Int)
withSpaceForEach = do
  contents <- arbitrary
  capacity <- atLeast (safeMaximum $ map size contents)
  return (contents, capacity)
  where safeMaximum [] = 0
        safeMaximum xs = maximum xs

thatCanHold :: Sized a => a -> Gen Int
thatCanHold = atLeast . size

atLeast :: Int -> Gen Int
atLeast n = choose (n, 1024*1024)

data Content = MetaData | RawData ByteString deriving (Eq, Ord, Show, Read)

instance Sized Content where
  size (MetaData) = 0
  size (RawData bytes) = size bytes

instance Arbitrary Content where
  arbitrary = oneof [return MetaData,
                     RawData <$> (C8.pack <$> letters)]
    where letters = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

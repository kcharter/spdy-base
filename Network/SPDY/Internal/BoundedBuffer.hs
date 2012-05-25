{-# LANGUAGE FlexibleInstances #-}

{- | Bounded buffers.

A bounded buffer is a queue of sized values in which the sum of
the sizes must be no more than a fixed capacity. These are intended
to help implement per-stream flow control and buffering in a SPDY
endpoint.

-}

module Network.SPDY.Internal.BoundedBuffer (Sized(..),
                                            BoundedBuffer,
                                            new,
                                            add,
                                            tryAdd,
                                            remove,
                                            totalCapacity,
                                            remainingCapacity) where

import Control.Concurrent.MSem (MSem)
import qualified Control.Concurrent.MSem as MSem
import Control.Concurrent.MSemN (MSemN)
import qualified Control.Concurrent.MSemN as MSemN
import Control.Exception (mask_)
import Control.Monad (liftM4, when, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable
import qualified Data.Foldable as F
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S

-- | Types with a size.
class Sized a where
  size :: a -> Int

instance Sized ByteString where
  size = B.length

instance (Sized a, Foldable f, Functor f) => Sized (f a) where
  size = F.sum . fmap size

-- | A FIFO collection of sized values, bounded by a total capacity.
data BoundedBuffer a =
  BoundedBuffer { bbChunks :: IORef (Seq a)
                , bbChunkCount :: MSem Int
                , bbFreeSpace :: MSemN Int
                , bbOriginalCapacity :: Int }

-- | Creates a new empty buffer with a given total capacity.
new :: Sized a => Int -> IO (BoundedBuffer a)
new capacity =
  liftM4 BoundedBuffer (newIORef S.empty) (MSem.new 0) (MSemN.new capacity) (return capacity)

-- | Adds a chunk to the buffer. Raises an error if the size
-- of the chunk is greater than the total capacity of the
-- buffer. Blocks if there is insufficient remaining capacity, until
-- another thread removes enough chunks to free the required space.
add :: Sized a => BoundedBuffer a -> a -> IO ()
add bb chunk = void $ addGeneral bb chunk True

-- | Attempts to add a chunk to the buffer, and indicates whether it
-- was successful. May block momentarily, but not indefinitely.
tryAdd :: Sized a => BoundedBuffer a -> a -> IO Bool
tryAdd bb chunk = addGeneral bb chunk False

-- | Adds a chunk, optionally waiting until space is
-- available. Returns 'True' if the chunk was added, 'False'
-- otherwise.
addGeneral :: Sized a => BoundedBuffer a -> a -> Bool -> IO Bool
addGeneral bb chunk waitForSpace = do
  ensurePossibleChunk bb chunk
  -- Using mask_ here should be sufficient to prevent an asynchronous
  -- exception from corrupting the state of the buffer, for three
  -- reasons. First, according to the SafeSemaphore docs MSemN.waitF
  -- can be interrupted, but it guarantees that it will not lose any
  -- of the semaphore quantity when this happens. Second, according to
  -- the Control.Exception docs, atomicModifyIORef is not
  -- interruptible. Third, according to the SafeSemaphore docs,
  -- MSem.signal is not interruptible. Therefore, the only part that
  -- can be interrupted by an asynchronous exception is MSemN.waitF,
  -- and in that case none of the semaphore quantity is taken, and the
  -- buffer will not add a chunk or signal the arrival of chunk.
  mask_ $ do
    (_, sufficient) <- MSemN.waitF (bbFreeSpace bb) demand
    when sufficient $ do
      atomicModifyIORef (bbChunks bb) (\chunks -> (chunks |> chunk, ()))
      MSem.signal (bbChunkCount bb)
    return sufficient
    where demand avail = let sufficientNow = avail >= n
                             n = size chunk
                             sufficient = sufficientNow || waitForSpace
                         in (if sufficient then n else 0, sufficient)

ensurePossibleChunk bb chunk =
  when (n > totalCapacity bb)
    (error ("Can't insert chunk of size " ++ show n ++
            " in a buffer with total capacity " ++
            show (totalCapacity bb)))
  where n = size chunk

-- | Removes the next available chunk from the buffer, blocking if the
-- buffer is empty.
remove :: Sized a => BoundedBuffer a -> IO a
remove bb = mask_ $ do
  -- mask_ should be sufficient to prevent asynchronous exceptions
  -- from corrupting the state of the buffer, for reasons similar to
  -- those described in 'addGeneral' above.
  MSem.wait (bbChunkCount bb)
  chunk <- atomicModifyIORef (bbChunks bb) takeFirst
  MSemN.signal (bbFreeSpace bb) (size chunk)
  return chunk
  where takeFirst chunks =
          let (first, rest) = S.splitAt 1 chunks
          in if S.length first > 0
             then let chunk = S.index first 0
                  in (rest, chunk)
             else error "Empty bounded buffer didn't block removal. This shouldn't happen."

-- | The total capacity of the buffer, i.e. the remaining capacity
-- when the buffer is empty.
totalCapacity :: BoundedBuffer a -> Int
totalCapacity = bbOriginalCapacity

-- | The current remaining capacity of the buffer. This is a snapshot
-- and may be invalid immediately afterward.
remainingCapacity :: Sized a => BoundedBuffer a -> IO Int
remainingCapacity bb = do
  chunks <- readIORef (bbChunks bb)
  return $! bbOriginalCapacity bb - F.sum (fmap size chunks)
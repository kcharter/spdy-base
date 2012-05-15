{- | Bounded buffers.

A bounded buffer is a queue of strict byte strings in which the sum of
the lengths must be no more than a fixed capacity. These are intended
to help implement per-stream flow control and buffering in a SPDY
endpoint.

The `BoundedBuffer` type could be generalized to any chunk type that
supports a length operation, but that's more than is necessary here. I
also suspect that I'm reinventing a wheel that is probably
better-implemented elsewhere (some kind of bounded channel perhaps)
but I haven't found a replacement yet.

-}

module Network.SPDY.Internal.BoundedBuffer (BoundedBuffer,
                                            new,
                                            add,
                                            remove,
                                            totalCapacity,
                                            remainingCapacity) where

import Control.Concurrent.QSem
import Control.Concurrent.QSemN
import Control.Monad (liftM4, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Foldable as F
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S

-- | A FIFO collection of byte strings, bounded by a total capacity.
data BoundedBuffer =
  BoundedBuffer { bbChunks :: IORef (Seq ByteString)
                , bbChunkCount :: QSem
                , bbFreeSpace :: QSemN
                , bbOriginalCapacity :: Int }

-- | Creates a new empty buffer with a given total capacity.
new :: Int -> IO BoundedBuffer
new capacity =
  liftM4 BoundedBuffer (newIORef S.empty) (newQSem 0) (newQSemN capacity) (return capacity)

-- | Adds a chunk of data to a buffer. Raises an error if the length
-- of the chunk is greater than the total capacity of the
-- buffer. Blocks if there is insufficient remaining capacity, until
-- another thread removes enough chunks to free the required space.
add :: BoundedBuffer -> ByteString -> IO ()
add bb bytes = do
  when (n > totalCapacity bb)
    (error ("Can't insert " ++ show n ++
            " bytes in a buffer with total capacity " ++
            show (totalCapacity bb)))
  waitQSemN (bbFreeSpace bb) n
  atomicModifyIORef (bbChunks bb) (\chunks -> (chunks |> bytes, ()))
  signalQSem (bbChunkCount bb)
  where n = B.length bytes

-- | Removes the next available chunk from the buffer, blocking if the
-- buffer is empty.
remove :: BoundedBuffer -> IO ByteString
remove bb = do
  waitQSem (bbChunkCount bb)
  bytes <- atomicModifyIORef (bbChunks bb) takeFirst
  signalQSemN (bbFreeSpace bb) (B.length bytes)
  return bytes
  where takeFirst chunks =
          let (first, rest) = S.splitAt 1 chunks
          in if S.length first > 0
             then let bytes = S.index first 0
                  in (rest, bytes)
             else error "Empty bounded buffer didn't block removal. This shouldn't happen."

-- | The total capacity of the buffer, i.e. the remaining capacity
-- when the buffer is empty.
totalCapacity :: BoundedBuffer -> Int
totalCapacity = bbOriginalCapacity

-- | The current remaining capacity of the buffer. This is a snapshot
-- and may be invalid immediately afterward.
remainingCapacity :: BoundedBuffer -> IO Int
remainingCapacity bb = do
  chunks <- readIORef (bbChunks bb)
  return $! bbOriginalCapacity bb - F.sum (fmap B.length chunks)

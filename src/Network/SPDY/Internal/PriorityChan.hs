{- | A simple priority channel, essentially a priority queue where the
producers and consumers are in different threads.

A priority channel is unbounded. Instead of reading elements in FIFO
order, the consumer reads the lowest-priority element that is
currently available, blocking if there are no elements.

The interface here is tiny, confined just to uses envisioned in this package.

This module should be imported qualified to avoid name clashes. -}

module Network.SPDY.Internal.PriorityChan (PriorityChan,
                                           newChan,
                                           send,
                                           receive) where

import qualified Control.Concurrent.MSem as MSem
import Control.Exception (mask_)
import Data.IORef (IORef, newIORef, atomicModifyIORef)

import qualified Data.PriorityQueue.FingerTree as FT

data PriorityChan p a = PriorityChan {
  pcSem :: MSem.MSem Int,
  -- ^ A semaphore that has credits as long as the queue is non-empty.
  pcIORef :: IORef (FT.PQueue p a)
  -- ^ The underlying priority queue.
  }

-- | Creates a new, empty priority channel.
newChan :: Ord p => IO (PriorityChan p a)
newChan = do
  sem <- MSem.new 0
  ref <- newIORef (FT.empty)
  return PriorityChan { pcSem = sem, pcIORef = ref }

-- | Sends an element with a given priority on the channel. The new
-- element will appear after all those of lower or equal priority that
-- have yet to be received by a consumer.
send :: Ord p => p -> a -> PriorityChan p a -> IO ()
send p x pq =
  -- note that we force evaluation of the new priority queue in this
  -- thread
  mask_ $ do atomicModifyIORef (pcIORef pq) $ \ftpq ->
               let ftpq' = FT.add p x ftpq in ftpq' `seq` (ftpq', ())
             MSem.signal (pcSem pq)

-- | Receives the minimum-priority element from the channel, blocking
-- until one is available.
receive :: Ord p => PriorityChan p a -> IO a
receive pq =
  mask_ (do MSem.wait (pcSem pq)
            atomicModifyIORef (pcIORef pq) $ \ftpq ->
              maybe (ftpq, Nothing) (\(x, ftpq') -> (ftpq', Just x)) (FT.minView ftpq))
  >>= maybe (error "Didn't block when receiving on an empty priority channel.") return

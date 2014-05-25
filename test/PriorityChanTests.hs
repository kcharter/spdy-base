module PriorityChanTests where

import Control.Arrow (second)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Monad (replicateM)
import Data.List (sortBy)
import Data.Ord (comparing)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Network.SPDY.Internal.PriorityChan (PriorityChan)
import qualified Network.SPDY.Internal.PriorityChan as PC

test :: Test
test = testGroup "Priority channel tests" [
  testProperty "lowest priority first" prop_lowestPriorityFirst,
  testProperty "lowest priority first, in batches" prop_lowestPriorityFirstEachBatch
  ]

prop_lowestPriorityFirst :: [(P,Int)] -> Property
prop_lowestPriorityFirst =
  ioProperty . ioprop_lowestPriorityFirst

ioprop_lowestPriorityFirst :: [(P,Int)] -> IO Bool
ioprop_lowestPriorityFirst inputs =
  do pc <- PC.newChan
     sendBatch pc inputs
     outputs <- receiveBatch pc
     return (outputs == expectedOutputs inputs)

prop_lowestPriorityFirstEachBatch :: [[(P,Int)]] -> Property
prop_lowestPriorityFirstEachBatch =
  ioProperty . ioprop_lowestPriorityFirstEachBatch

ioprop_lowestPriorityFirstEachBatch :: [[(P,Int)]] -> IO Bool
ioprop_lowestPriorityFirstEachBatch chunks =
  -- we use read and write semaphores to make sure the consumer
  -- completely fetches a batch before the producer sends elements of
  -- the next batch
  do writeSem <- newMVar ()
     readSem <- newEmptyMVar
     pc <- PC.newChan
     forkIO $
       mapM_ (\xs -> takeMVar writeSem >> sendBatch pc xs >> putMVar readSem ()) chunks
     outChunks <- replicateM (length chunks) $ do
       takeMVar readSem
       ys <- receiveBatch pc
       putMVar writeSem ()
       return ys
     return (outChunks == map expectedOutputs chunks)

toBatch :: [(P,a)] -> [(P, Maybe a)]
toBatch xs = map (second Just) xs ++ [(Hi, Nothing)]

sendBatch :: PriorityChan P (Maybe a) -> [(P, a)] -> IO ()
sendBatch pc =
  mapM_ (\(p,mi) -> PC.send p mi pc) . toBatch

receiveBatch :: PriorityChan P (Maybe a) -> IO [a]
receiveBatch pc = reverse `fmap` receiveBatch' []
  where receiveBatch' sofar =
          PC.receive pc >>=
          maybe (return sofar) (receiveBatch' . (:sofar))

expectedOutputs :: [(P,a)] -> [a]
expectedOutputs = map snd . sortBy (comparing fst)


data P = Lo | Med | Hi deriving (Eq, Ord, Enum, Bounded, Show)

instance Arbitrary P where
  arbitrary = elements [minBound..maxBound]

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Tests for 'Streams'.
--
-- The tests here use QuickCheck's monadic testing framework, defined
-- in the module 'Test.QuickCheck.Monadic', and try to follow some of
-- the ideas in the paper "Testing Monadic Code with QuickCheck" by
-- Koen Classen and John Hughes. In particular, I've created an
-- 'Action' type and tried exercising properties after achieving
-- reachable states by executing constrained sequences of actions.

module StreamTests (test) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, try)
import Control.Monad (void, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Data.List (foldl')
import Data.Maybe (isNothing, catMaybes)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Gen, arbitrary, choose, oneof, sized, vectorOf, forAll, Property)
import Test.QuickCheck.Monadic (PropertyM, run, pick, monadicIO, assert, forAllM, pre, stop)
import Test.QuickCheck.Property (failed, reason)

import Network.SPDY.Frames (StreamID(..), DeltaWindowSize)
import Network.SPDY.Internal.BoundedBuffer (Sized(..))
import Network.SPDY.Stream hiding (updateOutgoingWindowSize)
import qualified Network.SPDY.Stream as S

import Instances ()

test :: Test
test = testGroup "All stream model and stream tests" [
  testGroup "Stream model tests" [
     testProperty "Model adds content when there is room, updates local window size"
     prop_specAddIncoming,
     testProperty "Model refuses to add oversized content"
     prop_specRefuseAddIncoming
     ],
  testGroup "Stream tests" [
     testProperty "Pusher if and only if local half-open" $
     monadicIO $ propM_pusherIffLocalHalfOpen,
     testProperty "Reject incoming data chunks that are bigger than the remaining free space" $
     monadicIO $ propM_rejectIncomingDataBiggerThanFreeSpace,
     testProperty "Updating outgoing window size follows spec" $
     monadicIO $ propM_updateOutgoingWindowSize,
     testProperty "Pushing content follows spec" $
     monadicIO $ propM_push,
     testProperty "Buffering incoming content follows spec if there's room" $
     monadicIO $ propM_addIncoming,
     testProperty "Pulling buffered content follows spec" $
     monadicIO $ propM_pull
     ]
  ]

-- model tests, ordinary QuickCheck tests that confirm that the model
-- has the properties we desire

prop_specAddIncoming :: Property
prop_specAddIncoming =
  forAll genReachableModel $ \sm ->
  forAll (genIncomingContent sm) $ \c ->
  let (added, sm') = specAddIncoming c sm
  in added &&
     smBuffered sm' == smBuffered sm ++ [c] &&
     smLocalDWS sm' == smLocalDWS sm - size c

prop_specRefuseAddIncoming :: Property
prop_specRefuseAddIncoming =
  forAll genReachableModel $ \sm ->
  forAll (genOversizedIncomingContent sm) $ \c ->
  let (added, sm') = specAddIncoming c sm
  in not added && sm' == sm


-- Tests of the real stream implementation. Most of these are tests
-- for correspondence between the model and the real implementation.

propM_pusherIffLocalHalfOpen :: PropertyM IO ()
propM_pusherIffLocalHalfOpen = forAllM arbitrary $ \halfClosed -> do
  (_, maybePusher, _) <- doNothingStream halfClosed
  assert $ halfClosed == isNothing maybePusher

propM_rejectIncomingDataBiggerThanFreeSpace :: PropertyM IO ()
propM_rejectIncomingDataBiggerThanFreeSpace =
  forAllReachable $ \ts -> do
    freeSpace <- run $ smLocalDWS <$> abstract ts
    c    <- pick $ bytesBetween (freeSpace + 1) (2 * freeSpace + 2)
    last <- pick arbitrary
    queued <- run $ addIncomingData (tsStream ts) c last
    assert $ not queued

propM_updateOutgoingWindowSize :: PropertyM IO ()
propM_updateOutgoingWindowSize =
  implements' (fromTestStream genDeltaWindowSize)
  (flip updateOutgoingWindowSize) specUpdateOutgoingWindowSize

propM_push :: PropertyM IO ()
propM_push =
  implements' (fromTestStream genOutgoingContent) (flip push) specPush

propM_addIncoming :: PropertyM IO ()
propM_addIncoming =
  implements (fromTestStream $ \sm ->
               oneof [genIncomingContent sm
                     ,genOversizedIncomingContent sm])
  (flip addIncoming) specAddIncoming

propM_pull :: PropertyM IO ()
propM_pull =
  implementsIf
  (run . (not . null . smBuffered <$>) . abstract)
  (const $ return ())
  (const pull) (const specPull)

-- We approach testing a stream in one of the ways described by
-- Klassen and Hughes: assert that the stream implements a pure model,
-- under randomly reachable states. In order to generate randomly
-- reachable states, we defines a small language of stream actions,
-- and generate legal sequences of actions. Both the stream and the
-- abstract model of the stream can interpret these actions.

-- There is one wrinkle though: since a stream has callback actions
-- for sending content and window updates, we need to do a little work
-- to make use of the callbacks observable. Here, we embed a Stream in
-- a TestStream and supply it with callbacks that update parts of the
-- TestStream.

data TestStream =
  TestStream { tsStream :: Stream
             , tsPush :: StreamContent -> IO ()
             , tsPull :: IO StreamContent
             , tsRemoteBufSize :: Int
             , tsPushed :: IORef [StreamContent]
             , tsSentWUs :: IORef [DeltaWindowSize] }

newTestStream :: StreamID -> Int -> Int -> IO TestStream
newTestStream sid remoteBufSize localBufSize = do
  pushed <- newIORef []
  sentWUs <- newIORef []
  let opts = NewStreamOpts remoteBufSize localBufSize False recordOutgoing recordWindowUpdate doNothing
      recordOutgoing = doAppend pushed
      recordWindowUpdate = doAppend sentWUs
      doAppend xsRef x = atomicModifyIORef xsRef (\xs -> (xs ++ [x], ()))
  (s, Just push, pull) <- newStream sid opts
  return TestStream { tsStream = s
                    , tsPush = push
                    , tsPull = pull
                    , tsRemoteBufSize = remoteBufSize
                    , tsPushed = pushed
                    , tsSentWUs = sentWUs}

push :: TestStream -> StreamContent -> IO ()
push ts  = tsPush ts

addIncoming :: TestStream -> StreamContent -> IO Bool
addIncoming ts = forContent forHeaders forData
  where forHeaders hs last = addIncomingHeaders (tsStream ts) hs last >> return True
        forData bs last = addIncomingData (tsStream ts) bs last

addIncomingOrDie :: TestStream -> StreamContent -> IO ()
addIncomingOrDie ts sc = do
  added <- addIncoming ts sc
  unless added $ failedToAdd "test stream" sc =<< abstract ts

pull :: TestStream -> IO StreamContent
pull ts = tsPull ts

updateOutgoingWindowSize :: TestStream -> DeltaWindowSize -> IO ()
updateOutgoingWindowSize ts = S.updateOutgoingWindowSize (tsStream ts)

abstract :: TestStream -> IO StreamModel
abstract ts = do
  (remoteWindowSize, localWindowSize, buffered) <- snapshot $ tsStream ts
  pushed <- readIORef $ tsPushed ts
  sentWUs <- readIORef $ tsSentWUs ts
  return StreamModel { smRemoteBufSize = tsRemoteBufSize ts
                     , smRemoteDWS = remoteWindowSize
                     , smPushed = pushed
                     , smSentWUs = sentWUs
                     , smLocalDWS = localWindowSize
                     , smBuffered = buffered }

data StreamModel =
  StreamModel { smRemoteBufSize :: Int
                -- ^ Total remote buffering capacity, or the initial
                -- remote window size
              , smRemoteDWS :: Int
                -- ^ Remote data window size
              , smPushed :: [StreamContent]
                -- ^ Content sent on the stream
              , smSentWUs :: [DeltaWindowSize]
                -- ^ Window updates sent to the remote end
              , smLocalDWS :: Int
                -- ^ Local data window size
              , smBuffered :: [StreamContent]
                -- ^ Content in the local buffer
              }
  deriving (Eq, Show)

specPush :: StreamContent -> StreamModel -> StreamModel
specPush sc sm =
  forContent forHeaders forData sc
  where forHeaders _ _ = sm'
        forData c _ = sm' { smRemoteDWS = smRemoteDWS sm' - size c }
        sm' = sm { smPushed = smPushed sm ++ [sc] }

specAddIncoming :: StreamContent -> StreamModel -> (Bool, StreamModel)
specAddIncoming sc sm =
  forContent forHeaders forData sc
  where forHeaders _ _ = (True, sm')
        forData c _ = if size c <= smLocalDWS sm'
                      then (True, sm' { smLocalDWS = smLocalDWS sm' - size c })
                      else (False, sm)
        sm' = sm { smBuffered = smBuffered sm ++ [sc] }

specAddIncomingOrDie :: StreamContent -> StreamModel -> StreamModel
specAddIncomingOrDie sc sm =
  let (added, sm') = specAddIncoming sc sm
  in if added then sm' else failedToAdd "stream model" sc sm

specPull :: StreamModel -> (StreamContent, StreamModel)
specPull sm =
  (sc, forContent forHeaders forData sc)
  where sc = head $ smBuffered sm
        forHeaders _ _ = sm'
        forData c _ = sm' { smLocalDWS = smLocalDWS sm' + size c
                          , smSentWUs = smSentWUs sm ++ [ fromIntegral $ size c ] }
        sm' = sm { smBuffered = tail $ smBuffered sm }

specUpdateOutgoingWindowSize :: DeltaWindowSize -> StreamModel -> StreamModel
specUpdateOutgoingWindowSize wu sm =
  sm { smRemoteDWS = smRemoteDWS sm + fromIntegral wu }

failedToAdd :: String -> StreamContent -> StreamModel -> a
failedToAdd toWhat sc sm =
  error $
  "Failed to add incoming content of size " ++ show (size sc) ++
  " to " ++ toWhat ++
  " when there are " ++ show (smLocalDWS sm) ++
  " bytes free in the buffer."

data Action =
  Push StreamContent |
  AddIncoming StreamContent |
  Pull |
  UpdateOutgoingWindowSize DeltaWindowSize
  deriving (Eq, Show)

performAll :: TestStream -> [Action] -> IO ()
performAll s = mapM_ (performOne s)

performOne :: TestStream -> Action -> IO ()
performOne ts (Push sc) = push ts sc
performOne ts (AddIncoming sc) = addIncomingOrDie ts sc
performOne ts Pull = void (pull ts)
performOne ts (UpdateOutgoingWindowSize wu) = updateOutgoingWindowSize ts wu

specPerform :: StreamModel -> Action -> StreamModel
specPerform sm a = case a of
  Push sc -> specPush sc sm
  AddIncoming sc -> specAddIncomingOrDie sc sm
  Pull -> snd $ specPull sm
  UpdateOutgoingWindowSize wu -> specUpdateOutgoingWindowSize wu sm

-- | Generates a single non-blocking action and its effect on a stream
-- model.
step :: StreamModel -> Gen (StreamModel, Action)
step sm = do
  a <- oneof legalSteps
  return (specPerform sm a, a)
  where legalSteps =
          (if null (smBuffered sm) then [] else [return Pull]) ++
          [ Push <$> genOutgoingContent sm,
            AddIncoming <$> genIncomingContent sm,
            UpdateOutgoingWindowSize <$> genDeltaWindowSize sm ]

genOutgoingContent :: StreamModel -> Gen StreamContent
genOutgoingContent sm =
  oneof $ catMaybes [ Just genHeaders,
                      maybeGenData $ smRemoteDWS sm ]

genIncomingContent :: StreamModel -> Gen StreamContent
genIncomingContent sm =
  oneof $ catMaybes [ Just genHeaders,
                      maybeGenData $ smLocalDWS sm ]

genOversizedIncomingContent :: StreamModel -> Gen StreamContent
genOversizedIncomingContent sm =
  moreData <$> bytesBetween (localDWS + 1) (localDWS + 10) <*> arbitrary
    where localDWS = smLocalDWS sm

genHeaders :: Gen StreamContent
genHeaders = flip moreHeaders False <$> arbitrary

maybeGenData :: Int -> Maybe (Gen StreamContent)
maybeGenData max =
  if max > 0 then Just (flip moreData False <$> bytesBetween 1 max) else Nothing

genDeltaWindowSize :: StreamModel -> Gen DeltaWindowSize
genDeltaWindowSize sm = fromIntegral <$> choose (0, smRemoteBufSize sm - smRemoteDWS sm)

genReachableModel :: Gen StreamModel
genReachableModel = do
  rws <- windowSize
  lws <- windowSize
  let sm = StreamModel { smRemoteBufSize = rws
                       , smRemoteDWS = rws
                       , smPushed = []
                       , smSentWUs = []
                       , smLocalDWS = lws
                       , smBuffered = [] }
  foldl' specPerform sm <$> sized (actions sm)

-- | Generates a sequence of non-blocking actions.
actions :: StreamModel
           -- ^ An initial stream model, used to ensure that the
           -- actions will be non-blocking.
           -> Int
           -- ^ The length of the generated program.
           -> Gen [Action]
           -- ^ A sequence of actions that can be legally applied to
           -- the stream model in the order given.
actions sm n = snd <$> steps n sm
  where steps 0 sm = return (sm, [])
        steps n sm = do (sm', s) <- step sm
                        (sm'', ss) <- steps (n-1) sm'
                        return (sm'', s:ss)

commutes :: Eq a =>
            TestStream
            -> (TestStream -> IO a)
            -> (StreamModel -> (a, StreamModel))
            -> IO Bool
commutes ts tsOp modelOp = do
  initial <- abstract ts
  x <- tsOp ts
  final <- abstract ts
  return ((x, final) == modelOp initial)

forAllReachable :: (TestStream -> PropertyM IO ()) -> PropertyM IO ()
forAllReachable aPropM = do
  sid <- pick streamID
  rws <- pick windowSize
  lws <- pick windowSize
  ts  <- run $ newTestStream sid rws lws
  model <- run $ abstract ts
  as <- pick (sized $ actions model)
  failOnException $ performAll ts as
  aPropM ts

implementsIf :: Eq b =>
                (TestStream -> PropertyM IO Bool)
                -> (TestStream -> PropertyM IO a)
                -> (a -> TestStream -> IO b)
                -> (a -> StreamModel -> (b, StreamModel))
                -> PropertyM IO ()
implementsIf condition legalOperand tsOp modelOp =
  forAllReachable $ \ts -> do
    pre =<< condition ts
    x <- legalOperand ts
    assert =<< (failOnException $ commutes ts (tsOp x) (modelOp x))

implements :: Eq b =>
              (TestStream -> PropertyM IO a)
              -> (a -> TestStream -> IO b)
              -> (a -> StreamModel -> (b, StreamModel))
              -> PropertyM IO ()
implements = implementsIf (const $ return True)

implements' :: (TestStream -> PropertyM IO a)
               -> (a -> TestStream -> IO ())
               -> (a -> StreamModel -> StreamModel)
               -> PropertyM IO ()
implements' legalOperand tsOp modelOp =
  implements legalOperand tsOp (\x sm -> ((), modelOp x sm))

failOnException :: IO a -> PropertyM IO a
failOnException op =
  either stopExn return =<< (run $ try op)
  where stopExn e = stop $ failed { reason = "Exception: " ++ show (e :: SomeException) }

fromTestStream :: Show a => (StreamModel -> Gen a) -> TestStream -> PropertyM IO a
fromTestStream fromModel ts = run (abstract ts) >>= (pick . fromModel)

doNothingStream :: Bool
                   -> PropertyM IO (Stream, Maybe (StreamContent -> IO ()), IO StreamContent)
doNothingStream halfClosed = do
  sid <- pick streamID
  ws <- pick windowSize
  bs <- pick windowSize
  let opts = NewStreamOpts ws bs halfClosed doNothing doNothing doNothing
  run $ newStream sid opts

streamID :: Gen StreamID
streamID = StreamID <$> arbitrary

windowSize :: Gen Int
windowSize = choose (1, 64)

bytesBetween :: Int -> Int -> Gen ByteString
bytesBetween low hi = B.pack <$> vectorBetween low hi arbitrary

vectorBetween :: Int -> Int -> Gen a -> Gen [a]
vectorBetween low hi g = choose (low, hi) >>= flip vectorOf g

doNothing = const $ return ()

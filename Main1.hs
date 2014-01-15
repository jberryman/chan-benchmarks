{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
module Main
    where 

import Control.Concurrent.Async
import Control.Monad
import System.Environment

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TBQueue

import Control.Concurrent.MVar
import Data.IORef
import Criterion.Main
import Control.Exception(evaluate)

import qualified "chan-split-fast" Control.Concurrent.Chan.Split as S
import qualified "split-channel" Control.Concurrent.Chan.Split as SC
import Data.Primitive.MutVar
import Control.Monad.Primitive(PrimState)
import Data.Atomics.Counter
import System.Random
import System.Random.MWC

import Benchmarks

-- These tests initially taken from stm/bench/chanbench.hs, ported to
-- criterion, with some additions.
--
-- The original used CPP to avoid code duplication while also ensuring GHC
-- optimized the code in a realistic fashion. Here we just copy paste.

main = do 
  let n = 100000
--let n = 2000000  -- original suggested value, bugs if exceeded

  mv <- newEmptyMVar -- This to be left empty after each test
  mvFull <- newMVar undefined
  -- --
  -- mvWithFinalizer <- newEmptyMVar
  -- mkWeakMVar mvWithFinalizer $ return ()
  -- --
  -- mvFinalizee <- newMVar 'a'
  -- mvWithFinalizer <- newMVar ()
  -- mkWeakMVar mvWithFinalizer $
  --     modifyMVar_ mvFinalizee (const $ return 'b')
  -- --
  tmv <- newEmptyTMVarIO 
  tv <- newTVarIO undefined 
  ior <- newIORef undefined
  mutv <- newMutVar undefined

  atomic_counter <- newCounter 0

  -- to be left empty at emd of each test:
  chanEmpty <- newChan
  tchanEmpty <- newTChanIO
  tqueueEmpty <- newTQueueIO
  tbqueueEmpty <- newTBQueueIO 2
  (fastEmptyI,fastEmptyO) <- S.newSplitChan
  (splitchannelEmptyI,splitchannelEmptyO) <- SC.new

  -- random generators
  mwc_gen <- createSystemRandom
  sys_rand_gen <- newStdGen

  defaultMain $
        [ bgroup "Channel implementations" $
            -- Very artificial; just adding up the consts of the
            -- takes/puts/reads involved in getting a single message in and out
            [ bgroup "Latency micro-benchmark" $
                [ bench "Chan" (writeChan chanEmpty () >> readChan chanEmpty)
                , bench "TChan" (atomically (writeTChan tchanEmpty () >>  readTChan tchanEmpty))
                , bench "TQueue" (atomically (writeTQueue tqueueEmpty () >>  readTQueue tqueueEmpty))
                , bench "TBQueue" (atomically (writeTBQueue tbqueueEmpty () >>  readTBQueue tbqueueEmpty))
                , bench "chan-split-fast" (S.writeChan fastEmptyI () >> S.readChan fastEmptyO)
                , bench "split-channel" (SC.send splitchannelEmptyI () >> SC.receive splitchannelEmptyO)
                ]
            , bgroup ("Single-thread throughput with "++show n++" messages") $
                -- some pure operations we'd like a rough measurement for, e.g.
                -- the TQueue performs a reverse [1..n] in a test run, so we'd
                -- like an idea of how much that factor affects throughput.
                [ bgroup "For scale" $
                      [ bench "reverse [1..n]" $ nf (\n'-> reverse [1..n']) n
                      , bench "reverse replicate n 1" $ nf (\n'-> replicate n' (1::Int)) n
                      ]
                , bgroup "Chan" $
                      -- original tests from chanbench.hs
                      [ bench "sequential write all then read all" $ runtestChan1 n
                      , bench "repeated write some, read some" $ runtestChan2 n
                      ]
                , bgroup "TChan" $
                      [ bench "sequential write all then read all" $ runtestTChan1 n
                      , bench "repeated write some, read some" $ runtestTChan2 n
                      ]
                , bgroup "TQueue" $
                      [ bench "sequential write all then read all" $ runtestTQueue1 n
                      , bench "repeated write some, read some" $ runtestTQueue2 n
                      ]
                , bgroup "TBQueue" $
                      [ bench "sequential write all then read all" $ runtestTBQueue1 n
                      , bench "repeated write some, read some" $ runtestTBQueue2 n
                      ]
                -- OTHER CHAN IMPLEMENTATIONS:
                , bgroup "chan-split-fast" $
                      [ bench "sequential write all then read all" $ runtestSplitChan1 n
                      , bench "repeated write some, read some" $ runtestSplitChan2 n
                      ]
                , bgroup "split-channel" $
                      [ bench "sequential write all then read all" $ runtestSplitChannel1 n
                      , bench "repeated write some, read some" $ runtestSplitChannel2 n
                      ]
                ]
            ]
        , bgroup "Forking, context switches, and Misc. on a single core" $
            [ bench "forkIO" (forkIO (return ()) >> return ())
            , bench "put,take MVar" $ do
                putMVar mv '0'
                takeMVar mv
            , bench "put,takeMVar + forkIO + 2 context switches" $ do
                forkIO $ putMVar mv '0'
                takeMVar mv
            , bench "getNumCapabilities" getNumCapabilities
            , bench "myThreadId" myThreadId

            , bench "myThreadId >>= threadCapability" $ myThreadId >>= threadCapability
            -- It may not be possible to re-use generators for our applications of random, so:
            , bench "random new_gen" $ newStdGen
            , bench "random Int range: (1,8)" $ whnf (randomR (1 :: Int, 8)) sys_rand_gen
            , bench "mwc-random new_gen" $ createSystemRandom
            , bench "mwc-random Int range: (1,8)" $ ((uniformR (1 :: Int, 8) mwc_gen) :: IO Int)
            ]

        , bgroup "Var primitives" $
            [ bgroup "For scale" $ 
                [ bench "mod" $ nf (2147483647 `mod`) (8 :: Int)
                , bench "rem" $ nf (2147483647 `rem`) (8 :: Int)
                ]
            , bgroup "atomic-primops (puts on safety goggles...)" $ 
                [ bench "newCounter" $ newCounter 0
                , bench "incrCounter 1" $ incrCounter 1 atomic_counter
                , bench "incrCounter 1024" $ incrCounter 1024 atomic_counter
                ]
            , bgroup "IORef" $ 
                [ bench "newIORef Char" $ (newIORef '0')
                , bench "writeIORef" $ (writeIORef ior '1')
                , bench "readIORef" $ (readIORef ior)
                , bench "modifyIORef' (i.e. readIORef + writeIORef)" (modifyIORef' ior $ const '2')
                , bench "atomicModifyIORef' (i.e. (in GHC) strict atomicModifyMutVar)" $ (atomicModifyIORef' ior $ const ('3','3'))
                , bench "atomicModifyIORef (i.e. (in GHC) atomicModifyMutVar)" $ (atomicModifyIORef ior $ const ('3','3'))
                ]
            , bgroup "MVar" $
                [ bench "newEmptyMVar" $ newEmptyMVar
                , bench "newEmptyMVar + putMVar (i.e. newMVar Char)" (newEmptyMVar >>= \v->putMVar v '0')
                , bench "putMVar + takeMVar" $ (putMVar mv '1' >> takeMVar mv)
                , bench "modifyMVarMasked_ (i.e. takeMVar + putMVar + exception-handling)" $ (modifyMVarMasked_ mvFull (const $ return '2'))
                , bench "newMVar + mkWeakMVar with finalizer" $ (newMVar '1' >>= flip mkWeakMVar (return ()))
                -- These show no effect of a finalizer:
                -- , bench "on MVar with finalizer: putMVar, takeMVar" $ (putMVar mvWithFinalizer '1' >> takeMVar mvWithFinalizer)
                -- , bench "On target of an MVar finalizer: takeMVar, putMVar" $ (takeMVar mvFinalizee >>= putMVar mvFinalizee)
                ]

            , bgroup "TVar" $
                [ bench "newTVarIO ()" $ (newTVarIO ())
                , bench "atomically writeTVar" $ (atomically $ writeTVar tv '1')
                , bench "atomically readTVar" $ (atomically $ readTVar tv)
                , bench "readTVarIO" $ (readTVarIO tv)
                , bench "atomically modifyTVar' (i.e. atomically (readTVar + writeTVar))" $ (atomically $ modifyTVar' tv (const '2'))
                , bench "(atomically writeTVar) + (atomically readTVar)" $ ((atomically $ writeTVar tv '1') >> (atomically $ readTVar tv))
                ]
            , bgroup "TMVar" $
                [ bench "newEmptyTMVar (i.e. newTVarIO)" $ newEmptyTMVarIO
                , bench "atomically (newEmptyTMVar + putTMVar) (i.e. newTVar + readTVar + writeTVar,retry)" (atomically (newEmptyTMVar >>= \v->putTMVar v ()))
                , bench "atomically (putTMVar + takeTMVar)" $ (atomically $ (putTMVar tmv '1' >> takeTMVar tmv))
                , bench "(atomically putTMVar) + (atomically takeTMVar)" $ ((atomically $ putTMVar tmv '1') >> (atomically $ takeTMVar tmv))
                ]
            , bgroup "MutVar" $
                [ bench "newMutVar ()" $ (newMutVar () :: IO (MutVar (PrimState IO) ()))
                , bench "writeMutVar" $ (writeMutVar mutv '1' :: IO ())
                , bench "readMutVar" $ (readMutVar mutv :: IO Char)
                , bench "atomicModifyMutVar" $ (atomicModifyMutVar mutv $ const ('2','2') :: IO Char)
                ]
            ]
        -- Some more tests of pure operations relevant to TQueue style dequeue
        -- performance.
        , bgroup "Misc" $
            [ bench "pure cons-composition append x10" $ nf testCompositionAppend 10
            , bench "pure cons then final reverse x10" $       nf testConsReverse 10
            , bench "pure cons-composition append x100" $ nf testCompositionAppend 100
            , bench "pure cons then final reverse x100" $       nf testConsReverse 100
            , bench "pure cons-composition append x10000" $ nf testCompositionAppend 10000
            , bench "pure cons then final reverse x10000" $       nf testConsReverse 10000

            , bench "pure cons-composition append and prepend x10" $ nf testCompositionAppendPrepend 10
            , bench "pure cons-composition append and prepend x100" $ nf testCompositionAppendPrepend 100
            , bench "pure cons-composition append and prepend x10000" $ nf testCompositionAppendPrepend 10000

            , bench "mvar-stored cons-composition append x10" $ nfIO $ testCompositionAppendInMVar 10
            , bench "mvar-stored cons then final reverse x10" $ nfIO $       testConsReverseInMVar 10
            , bench "mvar-stored cons-composition append x100" $ nfIO $ testCompositionAppendInMVar 100
            , bench "mvar-stored cons then final reverse x100" $ nfIO $       testConsReverseInMVar 100
            , bench "mvar-stored cons-composition append x10000" $ nfIO $ testCompositionAppendInMVar 10000
            , bench "mvar-stored cons then final reverse x10000" $ nfIO $       testConsReverseInMVar 10000
            
            , bench "storing mvar-stored cons-composition append x10" $ nfIO $ testStoreCompositionAppendInMVar 10
            , bench "storing mvar-stored cons then final reverse x10" $ nfIO $       testStoreConsReverseInMVar 10
            , bench "storing mvar-stored cons-composition append x100" $ nfIO $ testStoreCompositionAppendInMVar 100
            , bench "storing mvar-stored cons then final reverse x100" $ nfIO $       testStoreConsReverseInMVar 100
            , bench "storing mvar-stored cons-composition append x10000" $ nfIO $ testStoreCompositionAppendInMVar 10000
            , bench "storing mvar-stored cons then final reverse x10000" $ nfIO $       testStoreConsReverseInMVar 10000
            ]
        ]
  -- takeMVar mvWithFinalizer -- keep finalizer from actually running


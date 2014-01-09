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
  defaultMain $
        [ bgroup "Channel implementations" $
            [ bgroup "For scale" $
                  [ bench "reverse [1..n]" $ nf (\n'-> reverse [1..n']) n
                  , bench "reverse replicate n 1" $ nf (\n'-> replicate n' (1::Int)) n
                  ]
            , bgroup "Chan" $
                  -- original tests from chanbench.hs
                  [ bench "async 1 writer 1 reader" $ runtestChan0 n
                  , bench "sequential write all then read all" $ runtestChan1 n
                  , bench "repeated write some, read some" $ runtestChan2 n
                  -- new benchmarks
                  , bench "async 2 writers 2 readers" $ runtestChanAsync 2 2 n
                  , bench "async 3 writers 1 reader" $ runtestChanAsync 3 1 n
                  , bench "async 100 writers 1 reader" $ runtestChanAsync 100 1 n
                  , bench "contention: async 100 writers 100 readers" $ runtestChanAsync 100 100 n
                  ]
            , bgroup "TChan" $
                  [ bench "async 1 writer 1 reader" $ runtestTChan0 n
                  , bench "sequential write all then read all" $ runtestTChan1 n
                  , bench "repeated write some, read some" $ runtestTChan2 n
                  , bench "async 2 writers 2 readers" $ runtestTChanAsync 2 2 n
                  , bench "async 3 writers 1 reader" $ runtestTChanAsync 3 1 n
                  , bench "async 100 writers 1 reader" $ runtestTChanAsync 100 1 n
                  , bench "contention: async 100 writers 100 readers" $ runtestTChanAsync 100 100 n
                  ]
            , bgroup "TQueue" $
                  [ bench "async 1 writer 1 reader" $ runtestTQueue0 n
                  , bench "sequential write all then read all" $ runtestTQueue1 n
                  , bench "repeated write some, read some" $ runtestTQueue2 n
                  , bench "async 2 writers 2 readers" $ runtestTQueueAsync 2 2 n
                  , bench "async 3 writers 1 reader" $ runtestTQueueAsync 3 1 n
                  , bench "async 100 writers 1 reader" $ runtestTQueueAsync 100 1 n
                  , bench "contention: async 100 writers 100 readers" $ runtestTQueueAsync 100 100 n
                  ]
            , bgroup "TBQueue" $
                  [ bench "async 1 writer 1 reader" $ runtestTBQueue0 n
                  , bench "sequential write all then read all" $ runtestTBQueue1 n
                  , bench "repeated write some, read some" $ runtestTBQueue2 n
                  , bench "async 2 writers 2 readers" $ runtestTBQueueAsync 2 2 n
                  , bench "async 3 writers 1 reader" $ runtestTBQueueAsync 3 1 n
                  , bench "async 100 writers 1 reader" $ runtestTBQueueAsync 100 1 n
                  , bench "contention: async 100 writers 100 readers" $ runtestTBQueueAsync 100 100 n
                  ]
            -- OTHER CHAN IMPLEMENTATIONS:
            , bgroup "chan-split-fast" $
                  [ bench "async 1 writer 1 reader" $ runtestSplitChan0 n
                  , bench "sequential write all then read all" $ runtestSplitChan1 n
                  , bench "repeated write some, read some" $ runtestSplitChan2 n
                  , bench "async 2 writers 2 readers" $ runtestSplitChanAsync 2 2 n
                  , bench "async 3 writers 1 reader" $ runtestSplitChanAsync 3 1 n
                  , bench "async 100 writers 1 reader" $ runtestSplitChanAsync 100 1 n
                  , bench "contention: async 100 writers 100 readers" $ runtestSplitChanAsync 100 100 n
                  ]
            , bgroup "split-channel" $
                  [ bench "async 1 writer 1 reader" $ runtestSplitChannel0 n
                  , bench "sequential write all then read all" $ runtestSplitChannel1 n
                  , bench "repeated write some, read some" $ runtestSplitChannel2 n
                  , bench "async 2 writers 2 readers" $ runtestSplitChannelAsync 2 2 n
                  , bench "async 3 writers 1 reader" $ runtestSplitChannelAsync 3 1 n
                  , bench "async 100 writers 1 reader" $ runtestSplitChannelAsync 100 1 n
                  , bench "contention: async 100 writers 100 readers" $ runtestSplitChannelAsync 100 100 n
                  ]
            ]
        , bgroup "Forking, context switches, and Misc." $
            [ bench "forkIO" (forkIO (return ()) >> return ())
            , bench "put/take MVar" $ do
                putMVar mv '0'
                takeMVar mv
            , bench "put/takeMVar + forkIO + 2 context switches" $ do
                forkIO $ putMVar mv '0'
                takeMVar mv
            , bench "getNumCapabilities" getNumCapabilities
            , bench "myThreadId" myThreadId
            ]

        , bgroup "Var primitives" $
            [ bgroup "IORef" $ 
                [ bench "newIORef Char" $ (newIORef '0')
                , bench "writeIORef" $ (writeIORef ior '1')
                , bench "readIORef" $ (readIORef ior)
                , bench "modifyIORef' (i.e. readIORef + writeIORef)" (modifyIORef' ior $ const '2')
                , bench "atomicModifyIORef' (i.e. (in GHC) atomicModifyMutVar)" $ (atomicModifyIORef' ior $ const ('3','3')) -- fair comparison?
                ]
            , bgroup "MVar" $
                [ bench "newEmptyMVar" $ newEmptyMVar
                , bench "newEmptyMVar + putMVar (i.e. newMVar Char)" (newEmptyMVar >>= \v->putMVar v '0')
                , bench "putMVar + takeMVar" $ (putMVar mv '1' >> takeMVar mv)
                , bench "modifyMVarMasked_ (i.e. takeMVar + putMVar + exception-handling)" $ (modifyMVarMasked_ mvFull (const $ return '2'))
                , bench "newMVar + mkWeakMVar w/ finalizer" $ (newMVar '1' >>= flip mkWeakMVar (return ()))
                
                -- These show no effect of a finalizer:
                -- , bench "on MVar with finalizer: putMVar, takeMVar" $ (putMVar mvWithFinalizer '1' >> takeMVar mvWithFinalizer)
                -- , bench "On target of an MVar finalizer: takeMVar, putMVar" $ (takeMVar mvFinalizee >>= putMVar mvFinalizee)

                ]

            , bgroup "TMVar" $
                [ bench "newEmptyTMVar (i.e. newTVarIO)" $ newEmptyTMVarIO
                , bench "atomically (newEmptyTMVar + putTMVar) (i.e. newTVar + readTVar + writeTVar/retry)" (atomically (newEmptyTMVar >>= \v->putTMVar v ()))
                , bench "atomically (putTMVar + takeTMVar)" $ (atomically $ (putTMVar tmv '1' >> takeTMVar tmv))
                , bench "(atomically putTMVar) + (atomically takeTMVar)" $ ((atomically $ putTMVar tmv '1') >> (atomically $ takeTMVar tmv))
                ]

            , bgroup "TVar" $
                [ bench "newTVarIO ()" $ (newTVarIO ())
                , bench "atomically writeTVar" $ (atomically $ writeTVar tv '1')
                , bench "atomically readTVar" $ (atomically $ readTVar tv)
                , bench "atomically modifyTVar' (i.e. atomically (readTVar + writeTVar))" $ (atomically $ modifyTVar' tv (const '2'))
                , bench "(atomically writeTVar) + (atomically readTVar)" $ ((atomically $ writeTVar tv '1') >> (atomically $ readTVar tv))
                ]

            , bgroup "MutVar" $
                [ bench "newMutVar ()" $ (newMutVar () :: IO (MutVar (PrimState IO) ()))
                , bench "writeMutVar" $ (writeMutVar mutv '1' :: IO ())
                , bench "readMutVar" $ (readMutVar mutv :: IO Char)
                , bench "atomicModifyMutVar" $ (atomicModifyMutVar mutv $ const ('2','2') :: IO Char)
                ]
            ]
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


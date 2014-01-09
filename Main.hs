{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
module Main
    where 

import Control.Concurrent.Async
import Control.Monad
import System.Environment

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

-- These tests initially taken from stm/bench/chanbench.hs, ported to
-- criterion, with some additions.
--
-- The original used CPP to avoid code duplication while also ensuring GHC
-- optimized the code in a realistic fashion. Here we just copy paste.

main = do 
  let n = 100000
--let n = 2000000  -- original suggested value, bugs if exceeded
  mv <- newEmptyMVar
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
        , bgroup "Var primitives" $
            [ bgroup "IORef" $ 
                [ bench "newIORef ()" $ (newIORef ())
                , bench "writeIORef, readIORef" $ (writeIORef ior '1' >> readIORef ior)
                , bench "atomicModifyIORef" $ (atomicModifyIORef ior $ const ('2','2')) -- fair comparison?
                ]
            , bgroup "MVar" $
                [ bench "newMVar ()" $ (newMVar ())
                , bench "putMVar, takeMVar" $ (putMVar mv '1' >> takeMVar mv)
                -- I'd expect this to be comparable to above:
                -- , bench "modifyMVar_" $ (modifyMVar_ mv (const $ return '2')) -- TODO need full first
                , bench "newMVar; and make weak, with finalizer" $ (newMVar () >>= flip mkWeakMVar (return ()))
                
                -- These show no effect of a finalizer:
                -- , bench "on MVar with finalizer: putMVar, takeMVar" $ (putMVar mvWithFinalizer '1' >> takeMVar mvWithFinalizer)
                -- , bench "On target of an MVar finalizer: takeMVar, putMVar" $ (takeMVar mvFinalizee >>= putMVar mvFinalizee)

                ]

            , bgroup "TMVar" $
                [ bench "newTMVarIO ()" $ (newTMVarIO ())
                , bench "atomically: putTMVar, takeTMVar" $ (atomically $ (putTMVar tmv '1' >> takeTMVar tmv))
                ]

            , bgroup "TVar" $
                [ bench "newTVarIO ()" $ (newTVarIO ())
                , bench "atomically: writeTVar, readTVar" $ (atomically $ (writeTVar tv '1' >> readTVar tv))
                , bench "atomically: modifyTVar" $ (atomically $ modifyTVar tv (const '2'))
                ]

            , bgroup "MutVar" $
                [ bench "newMutVar ()" $ (newMutVar () :: IO (MutVar (PrimState IO) ()))
                , bench "writeMutVar, readMutVar" $ ((writeMutVar mutv '1' :: IO ()) >> readMutVar mutv)
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




runtestChan0, runtestChan1, runtestChan2 :: Int -> IO ()
runtestChan0 n = do
  c <- newChan
  b <- async $ replicateM_ n $ readChan c
  a <- async $ replicateM_ n $ writeChan c (1 :: Int)
  waitBoth a b
  return ()

runtestChan1 n = do
  c <- newChan
  replicateM_ n $ writeChan c (1 :: Int)
  replicateM_ n $ readChan c

runtestChan2 n = do
  c <- newChan
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ writeChan c (1 :: Int)
    replicateM_ n1000 $ readChan c


runtestChanAsync :: Int -> Int -> Int -> IO ()
runtestChanAsync writers readers n = do
  let nNice = n - rem n (lcm writers readers)
  c <- newChan
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ readChan c
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ writeChan c (1 :: Int)
  mapM_ wait rcvrs

-- ----------

runtestTChan0, runtestTChan1, runtestTChan2 :: Int -> IO ()
runtestTChan0 n = do
  c <- newTChanIO
  b <- async $ replicateM_ n $ atomically $ readTChan c
  a <- async $ replicateM_ n $ atomically $ writeTChan c (1 :: Int)
  waitBoth a b
  return ()
runtestTChan1 n = do
  c <- newTChanIO
  replicateM_ n $ atomically $ writeTChan c (1 :: Int)
  replicateM_ n $ atomically $ readTChan c
runtestTChan2 n = do
  c <- newTChanIO
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ atomically $ writeTChan c (1 :: Int)
    replicateM_ n1000 $ atomically $ readTChan c

runtestTChanAsync :: Int -> Int -> Int -> IO ()
runtestTChanAsync writers readers n = do
  let nNice = n - rem n (lcm writers readers)
  c <- newTChanIO
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ atomically $ readTChan c
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ atomically $ writeTChan c (1 :: Int)
  mapM_ wait rcvrs

-- ----------

runtestTQueue0, runtestTQueue1, runtestTQueue2 :: Int -> IO ()
runtestTQueue0 n = do
  c <- newTQueueIO
  b <- async $ replicateM_ n $ atomically $ readTQueue c
  a <- async $ replicateM_ n $ atomically $ writeTQueue c (1 :: Int)
  waitBoth a b
  return ()
runtestTQueue1 n = do
  c <- newTQueueIO
  replicateM_ n $ atomically $ writeTQueue c (1 :: Int)
  replicateM_ n $ atomically $ readTQueue c
runtestTQueue2 n = do
  c <- newTQueueIO
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ atomically $ writeTQueue c (1 :: Int)
    replicateM_ n1000 $ atomically $ readTQueue c

runtestTQueueAsync :: Int -> Int -> Int -> IO ()
runtestTQueueAsync writers readers n = do
  let nNice = n - rem n (lcm writers readers)
  c <- newTQueueIO
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ atomically $ readTQueue c
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ atomically $ writeTQueue c (1 :: Int)
  mapM_ wait rcvrs

-- ----------

runtestTBQueue0, runtestTBQueue1, runtestTBQueue2 :: Int -> IO ()
runtestTBQueue0 n = do
  c <- newTBQueueIO 4096
  b <- async $ replicateM_ n $ atomically $ readTBQueue c
  a <- async $ replicateM_ n $ atomically $ writeTBQueue c (1 :: Int)
  waitBoth a b
  return ()
runtestTBQueue1 n = do
  c <- newTBQueueIO n -- The original benchmark must have blocked indefinitely here, no?
  replicateM_ n $ atomically $ writeTBQueue c (1 :: Int)
  replicateM_ n $ atomically $ readTBQueue c
runtestTBQueue2 n = do
  c <- newTBQueueIO 4096
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ atomically $ writeTBQueue c (1 :: Int)
    replicateM_ n1000 $ atomically $ readTBQueue c


runtestTBQueueAsync :: Int -> Int -> Int -> IO ()
runtestTBQueueAsync writers readers n = do
  let nNice = n - rem n (lcm writers readers)
  c <- newTBQueueIO 4096
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ atomically $ readTBQueue c
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ atomically $ writeTBQueue c (1 :: Int)
  mapM_ wait rcvrs


-- OTHER CHAN IMPLEMENTATIONS:

-- chan-split-fast

runtestSplitChan0, runtestSplitChan1, runtestSplitChan2 :: Int -> IO ()
runtestSplitChan0 n = do
  (i,o) <- S.newSplitChan
  b <- async $ replicateM_ n $ S.readChan o
  a <- async $ replicateM_ n $ S.writeChan i (1 :: Int)
  waitBoth a b
  return ()

runtestSplitChan1 n = do
  (i,o) <- S.newSplitChan
  replicateM_ n $ S.writeChan i (1 :: Int)
  replicateM_ n $ S.readChan o

runtestSplitChan2 n = do
  (i,o) <- S.newSplitChan
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ S.writeChan i (1 :: Int)
    replicateM_ n1000 $ S.readChan o


runtestSplitChanAsync :: Int -> Int -> Int -> IO ()
runtestSplitChanAsync writers readers n = do
  let nNice = n - rem n (lcm writers readers)
  (i,o) <- S.newSplitChan
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ S.readChan o
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ S.writeChan i (1 :: Int)
  mapM_ wait rcvrs



-- split-channel

runtestSplitChannel0, runtestSplitChannel1, runtestSplitChannel2 :: Int -> IO ()
runtestSplitChannel0 n = do
  (i,o) <- SC.new
  b <- async $ replicateM_ n $ SC.receive o
  a <- async $ replicateM_ n $ SC.send i (1 :: Int)
  waitBoth a b
  return ()

runtestSplitChannel1 n = do
  (i,o) <- SC.new
  replicateM_ n $ SC.send i (1 :: Int)
  replicateM_ n $ SC.receive o

runtestSplitChannel2 n = do
  (i,o) <- SC.new
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ SC.send i (1 :: Int)
    replicateM_ n1000 $ SC.receive o


runtestSplitChannelAsync :: Int -> Int -> Int -> IO ()
runtestSplitChannelAsync writers readers n = do
  let nNice = n - rem n (lcm writers readers)
  (i,o) <- SC.new
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ SC.receive o
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ SC.send i (1 :: Int)
  mapM_ wait rcvrs



-- --------------------------
-- Misc Components

testCompositionAppend :: Int -> [Int]
testCompositionAppend n = (go id [1..n]) [] where
    go f [] = f
    go f (a:as) = go (f . (a:)) as

-- are appends just as cheap as prepends?
testCompositionAppendPrepend :: Int -> [Int]
testCompositionAppendPrepend n = (go id [1..n]) [] where
    go f [] = f
    go f (a:as) 
        | even a = go (f . (a:)) as
        | otherwise = go ((a:) . f) as

testConsReverse :: Int -> [Int]
testConsReverse n = reverse $ go [1..n] [] where 
    go [] as = as
    go (a:xs) as = go xs (a:as)

-- This is more realistic, eliminating any benefits from inlining and rewriting
-- we might get from above
testCompositionAppendInMVar :: Int -> IO [Int]
testCompositionAppendInMVar n = do
    v <- newMVar id
    mapM_ (go v) [1..n]
    fmap ($ []) $ takeMVar v
  where go v a = do
            f <- takeMVar v
            fa <- evaluate (f . (a:))
            putMVar v fa

testConsReverseInMVar :: Int -> IO [Int]
testConsReverseInMVar n = do
    v <- newMVar []
    mapM_ (go v) [1..n]
    fmap reverse $ takeMVar v
  where go v a = do
            zs <- takeMVar v
            azs <- evaluate (a:zs)
            putMVar v azs

-- get an idea of the impact on writers:
testStoreCompositionAppendInMVar :: Int -> IO ()
testStoreCompositionAppendInMVar n = do
    v <- newMVar id
    mapM_ (go v) [1..n]
  where go v a = do
            f <- takeMVar v
            fa <- evaluate (f . (a:))
            putMVar v fa

testStoreConsReverseInMVar :: Int -> IO ()
testStoreConsReverseInMVar n = do
    v <- newMVar []
    mapM_ (go v) [1..n]
  where go v a = do
            zs <- takeMVar v
            azs <- evaluate (a:zs)
            putMVar v azs

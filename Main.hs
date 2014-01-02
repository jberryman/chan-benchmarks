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

import qualified "chan-split-fast" Control.Concurrent.Chan.Split as S
import qualified "split-channel" Control.Concurrent.Chan.Split as SC
import Data.Primitive.MutVar

-- These tests initially taken from stm/bench/chanbench.hs, ported to
-- criterion, with some additions.
--
-- The original used CPP to avoid code duplication while also ensuring GHC
-- optimized the code in a realistic fashion. Here we just copy paste.

main = do 
  let n = 100000
--let n = 2000000  -- original suggested value, bugs if exceeded
  mv <- newEmptyMVar
  tmv <- newEmptyTMVarIO 
  tv <- newTVarIO undefined 
  ior <- newIORef undefined
  mutv <- newMutVar undefined
  defaultMain $
        [ bgroup "Channel implementations" $
            [ bgroup "Chan" $
                  -- original tests from chanbench.hs
                  [ bench "async 1 writer 1 reader" $ runtestChan0 n
                  , bench "sequential write all then read all" $ runtestChan1 n
                  , bench "repeated write some, read some" $ runtestChan2 n
                  -- new benchmarks
                  , bench "async 2 writers two readers" $ runtestChanAsync 2 2 n
                  , bench "async 3 writers 1 reader" $ runtestChanAsync 3 1 n
                  ]
            , bgroup "TChan" $
                  [ bench "async 1 writer 1 reader" $ runtestTChan0 n
                  , bench "sequential write all then read all" $ runtestTChan1 n
                  , bench "repeated write some, read some" $ runtestTChan2 n
                  , bench "async 2 writers two readers" $ runtestTChanAsync 2 2 n
                  , bench "async 3 writers 1 reader" $ runtestTChanAsync 3 1 n
                  ]
            , bgroup "TQueue" $
                  [ bench "async 1 writer 1 reader" $ runtestTQueue0 n
                  , bench "sequential write all then read all" $ runtestTQueue1 n
                  , bench "repeated write some, read some" $ runtestTQueue2 n
                  , bench "async 2 writers two readers" $ runtestTQueueAsync 2 2 n
                  , bench "async 3 writers 1 reader" $ runtestTQueueAsync 3 1 n
                  ]
            , bgroup "TBQueue" $
                  [ bench "async 1 writer 1 reader" $ runtestTBQueue0 n
                  , bench "sequential write all then read all" $ runtestTBQueue1 n
                  , bench "repeated write some, read some" $ runtestTBQueue2 n
                  , bench "async 2 writers two readers" $ runtestTBQueueAsync 2 2 n
                  , bench "async 3 writers 1 reader" $ runtestTBQueueAsync 3 1 n
                  ]
            -- OTHER CHAN IMPLEMENTATIONS:
            , bgroup "chan-split-fast" $
                  [ bench "async 1 writer 1 reader" $ runtestSplitChan0 n
                  , bench "sequential write all then read all" $ runtestSplitChan1 n
                  , bench "repeated write some, read some" $ runtestSplitChan2 n
                  , bench "async 2 writers two readers" $ runtestSplitChanAsync 2 2 n
                  , bench "async 3 writers 1 reader" $ runtestSplitChanAsync 3 1 n
            , bgroup "split-channel" $
                  -- original tests from chanbench.hs
                  [ bench "async 1 writer 1 reader" $ runtestSplitChannel0 n
                  , bench "sequential write all then read all" $ runtestSplitChannel1 n
                  , bench "repeated write some, read some" $ runtestSplitChannel2 n
                  -- new benchmarks
                  , bench "async 2 writers two readers" $ runtestSplitChannelAsync 2 2 n
                  , bench "async 3 writers 1 reader" $ runtestSplitChannelAsync 3 1 n
                  ]
                  ]
            ]
        , bgroup "Var primitives" $
              [ bench "writeIORef, readIORef" $ (writeIORef ior '1' >> readIORef ior)
              , bench "atomicModifyIORef" $ (atomicModifyIORef ior $ const ('2','2')) -- fair comparison?

              , bench "putMVar, takeMVar" $ (putMVar mv '1' >> takeMVar mv)

              , bench "atomically: putTMVar, takeTMVar" $ (atomically $ (putTMVar tmv '1' >> takeTMVar tmv))

              , bench "atomically: writeTVar, readTVar" $ (atomically $ (writeTVar tv '1' >> readTVar tv))

              , bench "writeMutVar, readMutVar" $ ((writeMutVar mutv '1' :: IO ()) >> readMutVar mutv)
              , bench "atomicModifyMutVar" $ (atomicModifyMutVar mutv $ const ('2','2') :: IO Char) -- fair comparison?
              ]
        ]




runtestChan0, runtestChan1, runtestChan2 :: Int -> IO ()
runtestChan0 n = do
  c <- newChan
  a <- async $ replicateM_ n $ writeChan c (1 :: Int)
  b <- async $ replicateM_ n $ readChan c
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
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ writeChan c (1 :: Int)
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ readChan c
  mapM_ wait rcvrs
  mapM_ wait senders -- for exceptions, I guess?

-- ----------

runtestTChan0, runtestTChan1, runtestTChan2 :: Int -> IO ()
runtestTChan0 n = do
  c <- newTChanIO
  a <- async $ replicateM_ n $ atomically $ writeTChan c (1 :: Int)
  b <- async $ replicateM_ n $ atomically $ readTChan c
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
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ atomically $ writeTChan c (1 :: Int)
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ atomically $ readTChan c
  mapM_ wait rcvrs
  mapM_ wait senders -- for exceptions, I guess?

-- ----------

runtestTQueue0, runtestTQueue1, runtestTQueue2 :: Int -> IO ()
runtestTQueue0 n = do
  c <- newTQueueIO
  a <- async $ replicateM_ n $ atomically $ writeTQueue c (1 :: Int)
  b <- async $ replicateM_ n $ atomically $ readTQueue c
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
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ atomically $ writeTQueue c (1 :: Int)
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ atomically $ readTQueue c
  mapM_ wait rcvrs
  mapM_ wait senders -- for exceptions, I guess?

-- ----------

runtestTBQueue0, runtestTBQueue1, runtestTBQueue2 :: Int -> IO ()
runtestTBQueue0 n = do
  c <- newTBQueueIO 4096
  a <- async $ replicateM_ n $ atomically $ writeTBQueue c (1 :: Int)
  b <- async $ replicateM_ n $ atomically $ readTBQueue c
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
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ atomically $ writeTBQueue c (1 :: Int)
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ atomically $ readTBQueue c
  mapM_ wait rcvrs
  mapM_ wait senders -- for exceptions, I guess?


-- OTHER CHAN IMPLEMENTATIONS:

-- chan-split-fast

runtestSplitChan0, runtestSplitChan1, runtestSplitChan2 :: Int -> IO ()
runtestSplitChan0 n = do
  (i,o) <- S.newSplitChan
  a <- async $ replicateM_ n $ S.writeChan i (1 :: Int)
  b <- async $ replicateM_ n $ S.readChan o
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
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ S.writeChan i (1 :: Int)
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ S.readChan o
  mapM_ wait rcvrs
  mapM_ wait senders -- for exceptions, I guess?



-- split-channel

runtestSplitChannel0, runtestSplitChannel1, runtestSplitChannel2 :: Int -> IO ()
runtestSplitChannel0 n = do
  (i,o) <- SC.new
  a <- async $ replicateM_ n $ SC.send i (1 :: Int)
  b <- async $ replicateM_ n $ SC.receive o
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
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ SC.send i (1 :: Int)
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ SC.receive o
  mapM_ wait rcvrs
  mapM_ wait senders -- for exceptions, I guess?


{-# LANGUAGE RankNTypes #-}
module Main
    where 

import Control.Concurrent.Async
import Control.Monad
import System.Environment

import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TBQueue

import Criterion.Main

-- 	$(GHC) -O2 chanbench.hs -o chanbench-$$i; done
--  for i in 0 1 2; do echo; echo === test $$i ===; 
--      for j in CHAN TCHAN TQUEUE TBQUEUE; do 
    --      printf "%-10s" $$j; 
    --      time ./chanbench-$$j $$i 2000000; 
    --done; done

main = do 
  let n = 2000000
  defaultMain $
        [ bgroup "Chan" $
              [ bench "async writes/reads" $ runtestChan0 n
              , bench "sequential write all then read all" $ runtestChan1 n
              , bench "repeated write some, read some" $ runtestChan2 n
              ]
        , bgroup "TChan" $
              [ bench "async writes/reads" $ runtestTChan0 n
              , bench "sequential write all then read all" $ runtestTChan1 n
              , bench "repeated write some, read some" $ runtestTChan2 n
              ]
        , bgroup "TQueue" $
              [ bench "async writes/reads" $ runtestTQueue0 n
              , bench "sequential write all then read all" $ runtestTQueue1 n
              , bench "repeated write some, read some" $ runtestTQueue2 n
              ]
        , bgroup "TBQueue" $
              [ bench "async writes/reads" $ runtestTBQueue0 n
              , bench "sequential write all then read all" $ runtestTBQueue1 n
              , bench "repeated write some, read some" $ runtestTBQueue2 n
              ]
        ]

   -- TODO:
   --    async 2 writers, two readers
   --    async 3 writers, one reader




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

-- ----------

runtestTBQueue0, runtestTBQueue1, runtestTBQueue2 :: Int -> IO ()
runtestTBQueue0 n = do
  c <- newTBQueueIO 4096
  a <- async $ replicateM_ n $ atomically $ writeTBQueue c (1 :: Int)
  b <- async $ replicateM_ n $ atomically $ readTBQueue c
  waitBoth a b
  return ()
runtestTBQueue1 n = do
  c <- newTBQueueIO 4096
  replicateM_ n $ atomically $ writeTBQueue c (1 :: Int)
  replicateM_ n $ atomically $ readTBQueue c
runtestTBQueue2 n = do
  c <- newTBQueueIO 4096
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ atomically $ writeTBQueue c (1 :: Int)
    replicateM_ n1000 $ atomically $ readTBQueue c


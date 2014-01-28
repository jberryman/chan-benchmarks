{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
module Benchmarks
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
import Control.Exception(evaluate)


import qualified "chan-split-fast" Control.Concurrent.Chan.Split as S
import qualified "split-channel" Control.Concurrent.Chan.Split as SC
import Data.Primitive.MutVar
import Control.Monad.Primitive(PrimState)

-- These tests initially taken from stm/bench/chanbench.hs, ported to
-- criterion, with some additions, and have now changed quite a bit.
--
-- The original used CPP to avoid code duplication while also ensuring GHC
-- optimized the code in a realistic fashion. Here we just copy paste.



runtestChan1, runtestChan2 :: Int -> IO ()
runtestChan1 n = do
  c <- newChan
  replicateM_ n $ writeChan c ()
  replicateM_ n $ readChan c

runtestChan2 n = do
  c <- newChan
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ writeChan c ()
    replicateM_ n1000 $ readChan c

runtestChanAsync :: Int -> Int -> Int -> IO ()
runtestChanAsync writers readers n = do
  let nNice = n - rem n (lcm writers readers)
  c <- newChan
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ readChan c
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ writeChan c ()
  mapM_ wait rcvrs

-- ----------

runtestTChan1, runtestTChan2 :: Int -> IO ()
runtestTChan1 n = do
  c <- newTChanIO
  replicateM_ n $ atomically $ writeTChan c ()
  replicateM_ n $ atomically $ readTChan c

runtestTChan2 n = do
  c <- newTChanIO
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ atomically $ writeTChan c ()
    replicateM_ n1000 $ atomically $ readTChan c

runtestTChanAsync :: Int -> Int -> Int -> IO ()
runtestTChanAsync writers readers n = do
  let nNice = n - rem n (lcm writers readers)
  c <- newTChanIO
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ atomically $ readTChan c
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ atomically $ writeTChan c ()
  mapM_ wait rcvrs

-- ----------

runtestTQueue1, runtestTQueue2 :: Int -> IO ()
runtestTQueue1 n = do
  c <- newTQueueIO
  replicateM_ n $ atomically $ writeTQueue c ()
  replicateM_ n $ atomically $ readTQueue c

runtestTQueue2 n = do
  c <- newTQueueIO
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ atomically $ writeTQueue c ()
    replicateM_ n1000 $ atomically $ readTQueue c

runtestTQueueAsync :: Int -> Int -> Int -> IO ()
runtestTQueueAsync writers readers n = do
  let nNice = n - rem n (lcm writers readers)
  c <- newTQueueIO
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ atomically $ readTQueue c
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ atomically $ writeTQueue c ()
  mapM_ wait rcvrs

-- ----------

runtestTBQueue1, runtestTBQueue2 :: Int -> IO ()
runtestTBQueue1 n = do
  c <- newTBQueueIO n -- The original benchmark must have blocked indefinitely here, no?
  replicateM_ n $ atomically $ writeTBQueue c ()
  replicateM_ n $ atomically $ readTBQueue c

runtestTBQueue2 n = do
  c <- newTBQueueIO 4096
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ atomically $ writeTBQueue c ()
    replicateM_ n1000 $ atomically $ readTBQueue c

runtestTBQueueAsync :: Int -> Int -> Int -> IO ()
runtestTBQueueAsync writers readers n = do
  let nNice = n - rem n (lcm writers readers)
  c <- newTBQueueIO 4096
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ atomically $ readTBQueue c
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ atomically $ writeTBQueue c ()
  mapM_ wait rcvrs


-- OTHER CHAN IMPLEMENTATIONS:

-- chan-split-fast

runtestSplitChan1, runtestSplitChan2 :: Int -> IO ()
runtestSplitChan1 n = do
  (i,o) <- S.newSplitChan
  replicateM_ n $ S.writeChan i ()
  replicateM_ n $ S.readChan o

runtestSplitChan2 n = do
  (i,o) <- S.newSplitChan
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ S.writeChan i ()
    replicateM_ n1000 $ S.readChan o


runtestSplitChanAsync :: Int -> Int -> Int -> IO ()
runtestSplitChanAsync writers readers n = do
  let nNice = n - rem n (lcm writers readers)
  (i,o) <- S.newSplitChan
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ S.readChan o
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ S.writeChan i ()
  mapM_ wait rcvrs



-- split-channel

runtestSplitChannel1, runtestSplitChannel2 :: Int -> IO ()
runtestSplitChannel1 n = do
  (i,o) <- SC.new
  replicateM_ n $ SC.send i ()
  replicateM_ n $ SC.receive o

runtestSplitChannel2 n = do
  (i,o) <- SC.new
  let n1000 = n `quot` 1000
  replicateM_ 1000 $ do
    replicateM_ n1000 $ SC.send i ()
    replicateM_ n1000 $ SC.receive o

runtestSplitChannelAsync :: Int -> Int -> Int -> IO ()
runtestSplitChannelAsync writers readers n = do
  let nNice = n - rem n (lcm writers readers)
  (i,o) <- SC.new
  rcvrs <- replicateM readers $ async $ replicateM_ (nNice `quot` readers) $ SC.receive o
  senders <- replicateM writers $ async $ replicateM_ (nNice `quot` writers) $ SC.send i ()
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

-- -------------------------------------------------------------------------

-- we'd like to know whether in practice contention can be reduced on a shared
-- counter by first doing a read, and only doing an atomicModify when the
-- counter is not seen to have been incremented yet (note: even so, by the time
-- we're in the atomic block it may have been incremented, in which case it's a
-- NOOP)
readMaybeAtomicModifyIORef :: Int -> IO ()
readMaybeAtomicModifyIORef n = do
    counter <- newIORef 0
    stack1 <- newIORef [] -- non-contentious work done on these:
    stack2 <- newIORef []
    let op stck = do cnt <- readIORef counter
                     atomicModifyIORef' stck (\st-> (cnt:st,()))
                     cnt' <- readIORef counter
                     if cnt' == cnt
                         then atomicModifyIORef' counter (\cnt1-> (if cnt1 == cnt then cnt+1 else cnt1, ()))
                         else return ()

    w1 <- async $ replicateM_ n $ op stack1
    w2 <- async $ replicateM_ n $ op stack2
    waitBoth w1 w2
    return ()

atomicMaybeModifyIORef :: Int -> IO ()
atomicMaybeModifyIORef n = do
    counter <- newIORef 0
    stack1 <- newIORef [] -- non-contentious work done on these:
    stack2 <- newIORef []
    let op stck = do cnt <- readIORef counter
                     atomicModifyIORef' stck (\st-> (cnt:st,()))
                     atomicModifyIORef' counter (\cnt1-> (if cnt1 == cnt then cnt+1 else cnt1, ()))

    w1 <- async $ replicateM_ n $ op stack1
    w2 <- async $ replicateM_ n $ op stack2
    waitBoth w1 w2
    return ()

-- variants with a less realistic payload, simulating higher contention with more writers:
readMaybeAtomicModifyIORefHiC :: Int -> IO ()
readMaybeAtomicModifyIORefHiC n = do
    counter <- newIORef 0
    let op  =     do cnt <- readIORef counter
                     evaluate (show $ sqrt cnt)
                     cnt' <- readIORef counter
                     if cnt' == cnt
                         then atomicModifyIORef' counter (\cnt1-> (if cnt1 == cnt then cnt+1 else cnt1, ()))
                         else return ()

    w1 <- async $ replicateM_ n $ op 
    w2 <- async $ replicateM_ n $ op 
    waitBoth w1 w2
    return ()

atomicMaybeModifyIORefHiC :: Int -> IO ()
atomicMaybeModifyIORefHiC n = do
    counter <- newIORef 0
    let op      = do cnt <- readIORef counter
                     evaluate (show $ sqrt cnt)
                     atomicModifyIORef' counter (\cnt1-> (if cnt1 == cnt then cnt+1 else cnt1, ()))

    w1 <- async $ replicateM_ n $ op 
    w2 <- async $ replicateM_ n $ op 
    waitBoth w1 w2
    return ()



-- Do atomicModifyIORefs block readers?
-- NO
readsAgainstAtomicModifyIORefs :: Int -> IO ()
readsAgainstAtomicModifyIORefs n = do
    cntr <- newIORef 0
    t <- async $ forever (atomicModifyIORef' cntr (\c-> (c+1,())))
    replicateM_ n (readIORef cntr >>= evaluate)
    cancel t

readsAgainstNonAtomicModify :: Int -> IO ()
readsAgainstNonAtomicModify n = do
    cntr <- newIORef 0
    t <- async $ forever (modifyIORef' cntr (\c-> c+1))
    replicateM_ n (readIORef cntr >>= evaluate)
    cancel t
    

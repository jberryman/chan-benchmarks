module Main
    where

-- Testing interleaving on IORefs and TVars under contention. Most interested
-- in 2 competing HECs

import Control.Concurrent.STM
import Control.Concurrent
import Data.IORef
import Control.Monad
import Data.List
import System.Environment

main = do
    [msgsS] <- getArgs

    let msgs = read msgsS :: Int -- per thread (with 2 threads)
    tmsgs <- testTVar msgs
    iormsgs <- testIORef msgs 

    -- 69% mean
    pprint $ analyze iormsgs
    --  9% mean
    pprint $ analyze tmsgs

pprint (prefix, rest) = 
    putStrLn ( (show $ round $ (prefix*100))++"% of messages in constant prefix, with "++(show $ round (rest*100))++"% interleaving for rest"  )

analyze :: [Int] -> (Float, Float)
analyze l@(a:as) = 
    -- strip off starting list
    let lenL = length l
        lSuff = dropWhile (/=a) as
        lenPrefix = length (a:takeWhile (==a) as)
     in ( fromIntegral lenPrefix / fromIntegral lenL
        , fromIntegral (flops lSuff) / fromIntegral (lenL - lenPrefix - 1) )

flops  = subtract 1 . length . group

testTVar :: Int -> IO [Int]
testTVar msgs = do
    st <- newTVarIO []
    start1 <- newEmptyMVar
    start2 <- newEmptyMVar
    vs <- mapM (\(n,start)-> do 
                        v <- newEmptyMVar 
                        forkIO $ takeMVar start >> (replicateM_ msgs $ atomically $ modifyTVar' st (n:)) >> putMVar v ()
                        return v
               ) [(1, start1),(2,start2)]
    mapM_ (flip putMVar ()) [start2,start1]
    mapM_ takeMVar vs -- wait
    readTVarIO st

testIORef :: Int -> IO [Int]
testIORef msgs = do
    st <- newIORef []
    start1 <- newEmptyMVar
    start2 <- newEmptyMVar
    vs <- mapM (\(n,start)-> do 
                        v <- newEmptyMVar 
                        forkIO $ takeMVar start >> (replicateM_ msgs $ atomicModifyIORef' st (\st'-> (n:st',()))) >> putMVar v ()
                        return v
               ) [(1, start1),(2,start2)]
    mapM_ (flip putMVar ()) [start2,start1]
    mapM_ takeMVar vs -- wait
    readIORef st

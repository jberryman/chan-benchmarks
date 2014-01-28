module Main
    where

import Control.Concurrent
import Control.Monad

-- this to be run on one thread (i.e. +RTS -N1) to observe what happens to the
-- fairness property of MVars as green threads exceed HECs.
--
-- Interesting:
-- https://ghc.haskell.org/trac/ghc/ticket/7606
-- https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Scheduler
main = do
    st <- newMVar []
    vs <- mapM (\n-> do  v <- newEmptyMVar
                         forkIO (replicateM_ 10 $ modifyMVar_ st (return . (n:)) >> putMVar v ())
                         return v
          ) [1..10]
    mapM_ takeMVar vs -- wait
    takeMVar st >>= print
    


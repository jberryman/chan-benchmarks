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

import GHC.Conc

import Benchmarks

-- These tests initially taken from stm/bench/chanbench.hs, ported to
-- criterion, with some additions.
--
-- The original used CPP to avoid code duplication while also ensuring GHC
-- optimized the code in a realistic fashion. Here we just copy paste.

main = do 
  let n = 100000
--let n = 2000000  -- original suggested value, bugs if exceeded

  procs <- getNumCapabilities
  let procs_div2 = procs `div` 2
  if procs_div2 >= 0 then return ()
                     else error "Run with RTS +N2 or more"

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

  counter_mvar <- newMVar (1::Int)
  counter_ioref <- newIORef (1::Int)
  counter_tvar <- newTVarIO (1::Int)
  counter_atomic_counter <- newCounter (1::Int)

  fill_empty_chan <- newChan
  fill_empty_tchan <- newTChanIO
  fill_empty_tqueue <- newTQueueIO
  fill_empty_tbqueue <- newTBQueueIO maxBound
  (fill_empty_fastI, fill_empty_fastO) <- S.newSplitChan
  (fill_empty_splitchannelI, fill_empty_splitchannelO) <- SC.new

  defaultMain $
        [ bgroup "Var primitives" $
            -- This gives us an idea of how long a lock is held by these atomic
            -- ops, and the effects of retry/blocking scheduling behavior.
            -- compare this with latency measure in Main1 to get the whole
            -- picture:
            -- Subtract the cost of:
            --   - 2 context switches
            --   - 4 newEmptyMVar
            --   - 4 takeMVar
            --   - 4 putMVar
            -- TODO: also test with N green threads per core.
            [ bgroup ("Throughput on "++(show n)++" concurrent atomic mods") $
                     -- just forks some threads all atomically modifying a variable:
                let {-# INLINE mod_test #-}
                    mod_test = mod_test_n n
                    {-# INLINE mod_test_n #-}
                    mod_test_n n' = \threads modf -> do
                      dones <- replicateM threads newEmptyMVar ; starts <- replicateM threads newEmptyMVar
                      mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n' `div` threads) modf >> putMVar done1 ()) $ zip starts dones
                      mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones

                 in [ bgroup "1 thread per HEC" $
                       [ bench "modifyMVar_" $ mod_test procs $
                          (modifyMVar_ counter_mvar (return . (+1)))

                        , bench "modifyMVarMasked_" $ mod_test procs $
                            (modifyMVarMasked_ counter_mvar (return . (+1)))
                        
                        , bench "atomicModifyIORef'" $ mod_test procs $
                            (atomicModifyIORef' counter_ioref (\x-> (x+1,()) ))

                        , bench "atomically modifyTVar'" $ mod_test procs $
                            (atomically $ modifyTVar' counter_tvar ((+1))) 

                        , bench "incrCounter (atomic-primops)" $ mod_test procs $
                            (incrCounter 1 counter_atomic_counter)
                        
                        -- I want to compare these with the same results above;
                        -- see also TVarExperiment:
                        -- , bench "atomicModifyIORef' x10" $ mod_test_n (10*n) procs $
                        --     (atomicModifyIORef' counter_ioref (\x-> (x+1,()) ))
                        -- , bench "atomically modifyTVar' x10" $ mod_test_n (10*n) procs $
                        --     (atomically $ modifyTVar' counter_tvar ((+1))) 
                        ]
                    , bgroup "2 threads per HEC" $
                       [ bench "modifyMVar_" $ mod_test (procs*2) $
                          (modifyMVar_ counter_mvar (return . (+1)))

                        , bench "modifyMVarMasked_" $ mod_test (procs*2) $
                            (modifyMVarMasked_ counter_mvar (return . (+1)))
                        
                        , bench "atomicModifyIORef'" $ mod_test (procs*2) $
                            (atomicModifyIORef' counter_ioref (\x-> (x+1,()) ))

                        , bench "atomically modifyTVar'" $ mod_test (procs*2) $
                            (atomically $ modifyTVar' counter_tvar ((+1))) 

                        , bench "incrCounter (atomic-primops)" $ mod_test (procs*2) $
                            (incrCounter 1 counter_atomic_counter)
                        ]
                    , bgroup "4 threads per HEC" $
                       [ bench "modifyMVar_" $ mod_test (procs*4) $
                          (modifyMVar_ counter_mvar (return . (+1)))

                        , bench "modifyMVarMasked_" $ mod_test (procs*4) $
                            (modifyMVarMasked_ counter_mvar (return . (+1)))
                        
                        , bench "atomicModifyIORef'" $ mod_test (procs*4) $
                            (atomicModifyIORef' counter_ioref (\x-> (x+1,()) ))

                        , bench "atomically modifyTVar'" $ mod_test (procs*4) $
                            (atomically $ modifyTVar' counter_tvar ((+1))) 

                        , bench "incrCounter (atomic-primops)" $ mod_test (procs*4) $
                            (incrCounter 1 counter_atomic_counter)
                        ]
                    , bgroup "8 threads per HEC" $
                       [ bench "modifyMVar_" $ mod_test (procs*8) $
                          (modifyMVar_ counter_mvar (return . (+1)))

                        , bench "modifyMVarMasked_" $ mod_test (procs*8) $
                            (modifyMVarMasked_ counter_mvar (return . (+1)))
                        
                        , bench "atomicModifyIORef'" $ mod_test (procs*8) $
                            (atomicModifyIORef' counter_ioref (\x-> (x+1,()) ))

                        , bench "atomically modifyTVar'" $ mod_test (procs*8) $
                            (atomically $ modifyTVar' counter_tvar ((+1))) 

                        , bench "incrCounter (atomic-primops)" $ mod_test (procs*8) $
                            (incrCounter 1 counter_atomic_counter)
                        ]
                    ]
            ]
            -- TODO: define these in terms of numCapabilities:
            -- 1 r thread 1 w thread: measuring r/w contention
            -- 2 w threads ONLY: meeasure w/w contention, THEN:
            -- 2 r threads ONLY: meeasure r/r contention
            -- more threads: measuring descheduling bottlenecks, context switching overheads (+ above)
            --    above better tested outside criterion, w/ eventlogging
            --    also test equivalents of above on 8-core
        , bgroup "Channel implementations" $
            [ bgroup ("Operations on "++(show n)++" messages") $
                [ bgroup "For scale" $
                      [ bench "reverse [1..n]" $ nf (\n'-> reverse [1..n']) n
                      , bench "reverse replicate n 1" $ nf (\n'-> replicate n' (1::Int)) n
                      ]
                , bgroup "Chan" $
                      -- this gives us a measure of effects of contention between
                      -- readers and writers when compared with single-threaded
                      -- version:
                      [ bench "async 1 writer 1 readers" $ runtestChanAsync 1 1 n
                      -- NOTE: this is a bit hackish, filling in one test and
                      -- reading in the other; make sure memory usage isn't
                      -- influencing mean:
                      --
                      -- This measures writer/writer contention, in this case I
                      -- think we see a lot of thread blocking/waiting delays
                      , bench ("async "++(show procs)++" writers") $ do
                          dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
                          mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (writeChan fill_empty_chan ()) >> putMVar done1 ()) $ zip starts dones
                          mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
                      -- This measures reader/reader contention:
                      , bench ("async "++(show procs)++" readers") $ do
                          dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
                          mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (readChan fill_empty_chan) >> putMVar done1 ()) $ zip starts dones
                          mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
                      -- This is measuring the effects of bottlenecks caused by
                      -- descheduling, context-switching overhead (forced my
                      -- fairness properties in the case of MVar), as well as
                      -- all of the above; this is probably less than
                      -- informative. Try threadscope on a standalone test:
                      , bench "contention: async 100 writers 100 readers" $ runtestChanAsync 100 100 n
                      ]
                , bgroup "TChan" $
                      [ bench "async 1 writers 1 readers" $ runtestTChanAsync 1 1 n
                      -- This measures writer/writer contention:
                      {- LIVELOCK!!!
                      , bench ("async "++(show procs)++" writers") $ do
                          dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
                          mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (atomically $ writeTChan fill_empty_tchan ()) >> putMVar done1 ()) $ zip starts dones
                          mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
                      -- This measures reader/reader contention:
                      , bench ("async "++(show procs)++" readers") $ do
                          dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
                          mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (atomically $ readTChan fill_empty_tchan) >> putMVar done1 ()) $ zip starts dones
                          mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
                      , bench "contention: async 100 writers 100 readers" $ runtestTChanAsync 100 100 n
                      -}
                      ]
                , bgroup "TQueue" $
                      [ bench "async 1 writers 1 readers" $ runtestTQueueAsync 1 1 n
                      -- This measures writer/writer contention:
                      , bench ("async "++(show procs)++" writers") $ do
                          dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
                          mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (atomically $ writeTQueue fill_empty_tqueue ()) >> putMVar done1 ()) $ zip starts dones
                          mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
                      -- This measures reader/reader contention:
                      , bench ("async "++(show procs)++" readers") $ do
                          dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
                          mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (atomically $ readTQueue fill_empty_tqueue) >> putMVar done1 ()) $ zip starts dones
                          mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
                      , bench "contention: async 100 writers 100 readers" $ runtestTQueueAsync 100 100 n
                      ]
                , bgroup "TBQueue" $
                      [ bench "async 1 writers 1 readers" $ runtestTBQueueAsync 1 1 n
                      -- This measures writer/writer contention:
                      , bench ("async "++(show procs)++" writers") $ do
                          dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
                          mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (atomically $ writeTBQueue fill_empty_tbqueue ()) >> putMVar done1 ()) $ zip starts dones
                          mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
                      -- This measures reader/reader contention:
                      , bench ("async "++(show procs)++" readers") $ do
                          dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
                          mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (atomically $ readTBQueue fill_empty_tbqueue) >> putMVar done1 ()) $ zip starts dones
                          mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
                      , bench "contention: async 100 writers 100 readers" $ runtestTBQueueAsync 100 100 n
                      ]
                -- OTHER CHAN IMPLEMENTATIONS:
                , bgroup "chan-split-fast" $
                      [ bench "async 1 writers 1 readers" $ runtestSplitChanAsync 1 1 n
                      -- This measures writer/writer contention:
                      , bench ("async "++(show procs)++" writers") $ do
                          dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
                          mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (S.writeChan fill_empty_fastI ()) >> putMVar done1 ()) $ zip starts dones
                          mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
                      -- This measures reader/reader contention:
                      , bench ("async "++(show procs)++" readers") $ do
                          dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
                          mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (S.readChan fill_empty_fastO) >> putMVar done1 ()) $ zip starts dones
                          mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
                      , bench "contention: async 100 writers 100 readers" $ runtestSplitChanAsync 100 100 n
                      ]
                , bgroup "split-channel" $
                      [ bench "async 1 writers 1 readers" $ runtestSplitChannelAsync 1 1 n
                      -- This measures writer/writer contention:
                      , bench ("async "++(show procs)++" writers") $ do
                          dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
                          mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (SC.send fill_empty_splitchannelI ()) >> putMVar done1 ()) $ zip starts dones
                          mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
                      -- This measures reader/reader contention:
                      , bench ("async "++(show procs)++" readers") $ do
                          dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
                          mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (SC.receive fill_empty_splitchannelO) >> putMVar done1 ()) $ zip starts dones
                          mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
                      , bench "contention: async 100 writers 100 readers" $ runtestSplitChannelAsync 100 100 n
                      ]
                ]
            ]
        ]
  -- to make sure the counter is actually being incremented!:
  cntv <- readCounter counter_atomic_counter
  putStrLn $ "Final counter val is "++(show cntv)

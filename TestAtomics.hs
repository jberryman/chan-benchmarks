import Control.Concurrent
import Data.Atomics.Counter
import Data.Atomics
import Data.IORef
import Control.Monad
import GHC.Conc
import Control.Exception(evaluate)

main = do
    testCAS_ABA_0
    testCAS_ABA_0_modAfterAtomicMod
    testCAS_ABA_0_modBefAtomicMod
    testCAS_ABA_1
    testCAS_ABA_2
    testCAS_ABA_3

    counterTest

counterTest = do
    n0 <- testAtomicCount newCounter readCounter incrCounter
    n1 <- testAtomicCount newMVar takeMVar (\n v-> modifyMVar_ v (evaluate . (+1)) )
    if n0 /= n1
        then putStrLn $ "Counter broken: expecting "++(show n1)++" got "++(show n0)
        else putStrLn "OK"

testAtomicCount new read incr = do
  let n = 1000000
  procs <- getNumCapabilities

  counter <- new (1::Int)
  dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
  mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (incr 1 counter) >> putMVar done1 ()) $ zip starts dones
  mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
  
  read counter

-- test ABA issue with these three cases:
--   ()
--   Bool
--       {-# NOINLINE True #-}
--   let true = True



-- returns False
testCAS_ABA_0 = do
    a <- newIORef ()
    ta <- readForCAS a
    atomicModifyIORef' a (\u-> (u,u))
    (res, _) <- casIORef a ta () 
    print res
{- same
testCAS_ABA_0_nonstrict = do
    a <- newIORef ()
    ta <- readForCAS a
    atomicModifyIORef a (\u-> (u,u))
    (res, _) <- casIORef a ta () 
    print res
testCAS_ABA_0_u = do
    a <- newIORef ()
    ta <- readForCAS a
    atomicModifyIORef' a (const ((),()))
    (res, _) <- casIORef a ta () 
    print res
testCAS_ABA_0_sameu = do
    let {-# NOINLINE u #-} 
        u = ()
    a <- newIORef u
    ta <- readForCAS a
    atomicModifyIORef' a (const (u,u))
    (res, _) <- casIORef a ta () 
    print res
-}

-- returns True
testCAS_ABA_1 = do
    a <- newIORef ()
    ta <- readForCAS a
    modifyIORef a (const ())   -- i.e. readIORef >> writeIORef
    (res, _) <- casIORef a ta () 
    print res

{- same
testCAS_ABA_1_casMod = do
    a <- newIORef ()
    ta <- readForCAS a
    atomicModifyIORefCAS_ a id
    (res, _) <- casIORef a ta () 
    print res
testCAS_ABA_1_id = do
    a <- newIORef ()
    ta <- readForCAS a
    modifyIORef a id   -- i.e. readIORef >> writeIORef
    (res, _) <- casIORef a ta () 
    print res
-}

-- returns True
-- ... so the issue isn't re-ordering of readForCas and the read in modifyIORef
-- in fact, no combination of the barriers provided seem to work.
testCAS_ABA_2 = do
    a <- newIORef ()
    ta <- readForCAS a
    loadLoadBarrier
    modifyIORef a (const ())   -- i.e. readIORef >> writeIORef
    (res, _) <- casIORef a ta () 
    print res

testCAS_ABA_3 = do
    barrier <- newIORef ()

    a <- newIORef ()
    ta <- readForCAS a

    atomicModifyIORef' barrier (\u-> (u,u))  -- just a barrier
    modifyIORef a (const ())   -- i.e. readIORef >> writeIORef
    atomicModifyIORef' barrier (\u-> (u,u))  -- just a barrier
    
    (res, _) <- casIORef a ta () 
    print res

-- INTERESTING!: /adding/ the modifyIORef /after/ the atomicModifyIORef causes this to return True!
testCAS_ABA_0_modAfterAtomicMod = do
    barrier <- newIORef ()

    a <- newIORef ()
    ta <- readForCAS a

    atomicModifyIORef' a (\u-> (u,u))
    modifyIORef a (const ())   -- i.e. readIORef >> writeIORef
    
    (res, _) <- casIORef a ta () 
    print res

-- ...whereas this one returns False again
testCAS_ABA_0_modBefAtomicMod = do
    barrier <- newIORef ()

    a <- newIORef ()
    ta <- readForCAS a

    modifyIORef a (const ())   -- i.e. readIORef >> writeIORef
    atomicModifyIORef' a (\u-> (u,u))
    
    (res, _) <- casIORef a ta () 
    print res

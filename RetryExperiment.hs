module Main
    where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad

import System.IO
import Debug.Trace

main = do
    hSetBuffering stdout NoBuffering
    noMansLand <- replicateM 998 $ newTVarIO 0
    t0 <- newTVarIO (1::Int)
    t999 <- newTVarIO (-1)
    let ts = t0:noMansLand++[t999]

    done <- atomically $ newTSem 0
    forkIO $ atomically $ nestedOrElseMap done ts

    -- need enough time here for nestedOrElseMap thread above to move past t0

    -- in this version, the modifications to t0 force nestedOrElseMap to be restarted
    forkIO (trace "starting vexing!" $ forever $ atomically $ (modifyTVar' t0 (+1) >> trace "vex" (return ())))
    -- in this version nestedOrElseMap causes this transaction to be restarted and never makes progress:
    --forkIO (atomically (trace "starting vexing!" $ forever $ (modifyTVar' t0 (+1) >> trace "vex" (return ()))))

    atomically $ waitTSem done
    putStrLn "No livelock! Did the t0 counter get incremented?: "
    atomically (readTVar t0) >>= print
   
nestedOrElseMap :: TSem -> [TVar Int] -> STM ()
nestedOrElseMap done ts = trace "nestedOrElseMap starting" $ foldl1 orElse $ map transaction $ zip [(1::Int)..] ts
    where transaction (cnt,v) = do
            n <- traceShow cnt $ readTVar v
            if n < 0 
                then trace "@" (modifyTVar' v (subtract 1)) >> signalTSem done
                else  retry

-- NOTE: this shows at least that we get livelock as we wait on the first transaction (it may also be executed; add Debug.Trace)
-- that's not really working...
--
-- CONSIDER:
-- The behavior we see ensures that all branches of orElse see the same view of
-- the same variables, but is overzealous! It should do validation for each
-- subtransaction by only checking oldest parent read of each variable *used*
-- in the transaction, and if any changed, then go up only to the last
-- inconsistency)
--     ""If both t1 and t2 execute retry then even though the effects of t1 are
--     thrown away, it could be that a change to a TVar that is only in the
--     access set of t1 will allow the whole transaction to succeed when it is
--     woken.  To solve this problem, when a branch on a nested transaction is
--     aborted the access set of the nested transaction is merged as a read set
--     into the parent TRec. Specifically if the TVar is in any TRec up the
--     chain of nested transactions it must be ignored, otherwise it is entered
--     as a new entry (retaining just the read) in the parent TRec.""
--
-- "aborted"? "read set"?
--     -- is this the culprit? No. Clearly we move on from that branch in test.
--     "A validation failure in the first branch aborts the entire transaction, not just the nested part"
--        "validation"?
--          "Before a transaction can make its effects visible to other threads
--          it must check that it has seen a consistent view of memory while it
--          was executing. Most of the work is done in
--          validate_and_acquire_ownership by checking that TVars hold their
--          expected values. "
--          
--     when commiting, do we force that the read set is unchanged?
-- 
-- Soooo
-- The writes to variables from other branches are causing a validation failure
-- and causing whole transaction to reset

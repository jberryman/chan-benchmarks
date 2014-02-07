import Control.Concurrent
import Data.Atomics.Counter
import Control.Monad
import GHC.Conc
import Control.Exception(evaluate)


main = do
    n0 <- test newCounter readCounter incrCounter
    n1 <- test newMVar takeMVar (\n v-> modifyMVar_ v (evaluate . (+1)) )
    if n0 /= n1
        then putStrLn $ "Counter broken: expecting "++(show n1)++" got "++(show n0)
        else putStrLn "OK"

test new read incr = do
  let n = 1000000
  procs <- getNumCapabilities

  counter <- new (1::Int)
  dones <- replicateM procs newEmptyMVar ; starts <- replicateM procs newEmptyMVar
  mapM_ (\(start1,done1)-> forkIO $ takeMVar start1 >> replicateM_ (n `div` procs) (incr 1 counter) >> putMVar done1 ()) $ zip starts dones
  mapM_ (\v-> putMVar v ()) starts ; mapM_ (\v-> takeMVar v) dones
  
  read counter

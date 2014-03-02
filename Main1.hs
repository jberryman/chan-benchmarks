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
import Control.Exception(evaluate,mask_)

import qualified "chan-split-fast" Control.Concurrent.Chan.Split as S
import qualified "split-channel" Control.Concurrent.Chan.Split as SC
import Data.Primitive.MutVar
import Control.Monad.Primitive(PrimState)
import Data.Atomics.Counter
import Data.Atomics
import System.Random
import System.Random.MWC

import Data.Array.IArray
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Primitive as P
-- TODO fix imports above
import qualified Data.Vector.Mutable as MVec

import System.Time(getClockTime)
import Data.Time.Clock.POSIX

-- Hack since these aren't currently working with ghc 7.8
#if MIN_VERSION_base(4,7,0)
#else
import qualified Data.Concurrent.Queue.MichaelScott as MS
#endif
import qualified Data.Concurrent.Deque.ChaseLev as CL

import Benchmarks

-- These tests initially taken from stm/bench/chanbench.hs, ported to
-- criterion, with some additions.
--
-- The original used CPP to avoid code duplication while also ensuring GHC
-- optimized the code in a realistic fashion. Here we just copy paste.

main = do 
  let n = 100000
--let n = 2000000  -- original suggested value, bugs if exceeded

  mv <- newEmptyMVar -- This to be left empty after each test
  mvFull <- newMVar '1'
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
  tv <- newTVarIO '1' 
  ior <- newIORef '1'
  mutv <- newMutVar '1'

  atomic_counter <- newCounter 0

  -- to be left empty at emd of each test:
#if MIN_VERSION_base(4,7,0)
#else
  lockfreeQEmpty <- MS.newQ
#endif
  chaselevQEmpty <- CL.newQ
  chanEmpty <- newChan
  tchanEmpty <- newTChanIO
  tqueueEmpty <- newTQueueIO
  tbqueueEmpty <- newTBQueueIO 2
  (fastEmptyI,fastEmptyO) <- S.newSplitChan
  (splitchannelEmptyI,splitchannelEmptyO) <- SC.new

  -- random generators
  mwc_gen <- createSystemRandom
  sys_rand_gen <- newStdGen

  let arr8 = listArray (0,7) [1..8] :: Array Int Int
  let arr16 = listArray (0,15) [1..16] :: Array Int Int
  let vec8 = V.fromList [1..8] :: V.Vector Int
  let vec16 = V.fromList [1..16] :: V.Vector Int
  mvec8 <- V.thaw vec8 -- :: V.MVector (PrimState IO) Int
  mvec16 <- V.thaw vec16 -- :: V.MVector (PrimState IO) Int
  parr8 <- P.newArray 8 (0::Int)
  parr16 <- P.newArray 16 (0::Int)
  iparr8 <- ((P.newArray 8 (0::Int)) >>= P.unsafeFreezeArray) :: IO (P.Array Int)
  iparr16 <- ((P.newArray 16 (0::Int)) >>= P.unsafeFreezeArray) :: IO (P.Array Int)

  ba16 <- (P.newByteArray (16 * P.sizeOf (0 :: Int)) :: IO (P.MutableByteArray (PrimState IO)))
  forM_ [0..15] $ \i-> P.writeByteArray ba16 i (0::Int)
  umvvec16 <- UMV.new 16 :: IO (UMV.IOVector Int)
  UMV.set umvvec16 0 :: IO ()

  defaultMain $
        [ bgroup "Channel implementations" $
            -- Very artificial; just adding up the consts of the
            -- takes/puts/reads involved in getting a single message in and out
            [ bgroup "Latency micro-benchmark" $
                [ bench "Chan" (writeChan chanEmpty () >> readChan chanEmpty)
                , bench "TChan" (atomically (writeTChan tchanEmpty () >>  readTChan tchanEmpty))
                , bench "TQueue" (atomically (writeTQueue tqueueEmpty () >>  readTQueue tqueueEmpty))
                , bench "TBQueue" (atomically (writeTBQueue tbqueueEmpty () >>  readTBQueue tbqueueEmpty))
                , bench "chan-split-fast" (S.writeChan fastEmptyI () >> S.readChan fastEmptyO)
                , bench "split-channel" (SC.send splitchannelEmptyI () >> SC.receive splitchannelEmptyO)
#if MIN_VERSION_base(4,7,0)
#else
                , bench "lockfree-queue" (MS.pushL lockfreeQEmpty () >> msreadR lockfreeQEmpty)
#endif
                , bench "chaselev-dequeue" (CL.pushL chaselevQEmpty () >> clreadR chaselevQEmpty)
                ]
            , bgroup ("Single-thread throughput with "++show n++" messages") $
                -- some pure operations we'd like a rough measurement for, e.g.
                -- the TQueue performs a reverse [1..n] in a test run, so we'd
                -- like an idea of how much that factor affects throughput.
                [ bgroup "For scale" $
                      [ bench "reverse [1..n]" $ nf (\n'-> reverse [1..n']) n
                      , bench "reverse replicate n 1" $ nf (\n'-> replicate n' (1::Int)) n
                      ]
                , bgroup "Chan" $
                      -- original tests from chanbench.hs
                      [ bench "sequential write all then read all" $ runtestChan1 n
                      , bench "repeated write some, read some" $ runtestChan2 n
                      ]
                , bgroup "TChan" $
                      [ bench "sequential write all then read all" $ runtestTChan1 n
                      , bench "repeated write some, read some" $ runtestTChan2 n
                      ]
                , bgroup "TQueue" $
                      [ bench "sequential write all then read all" $ runtestTQueue1 n
                      , bench "repeated write some, read some" $ runtestTQueue2 n
                      ]
                , bgroup "TBQueue" $
                      [ bench "sequential write all then read all" $ runtestTBQueue1 n
                      , bench "repeated write some, read some" $ runtestTBQueue2 n
                      ]
                -- OTHER CHAN IMPLEMENTATIONS:
                , bgroup "chan-split-fast" $
                      [ bench "sequential write all then read all" $ runtestSplitChan1 n
                      , bench "repeated write some, read some" $ runtestSplitChan2 n
                      ]
                , bgroup "split-channel" $
                      [ bench "sequential write all then read all" $ runtestSplitChannel1 n
                      , bench "repeated write some, read some" $ runtestSplitChannel2 n
                      ]
#if MIN_VERSION_base(4,7,0)
#else
                , bgroup "lockfree-queue" $
                      [ bench "sequential write all then read all" $ runtestLockfreeQueue1 n
                      , bench "repeated write some, read some" $ runtestLockfreeQueue2 n
                      ]
#endif
                , bgroup "chaselev-dequeue" $
                      [ bench "sequential write all then read all" $ runtestChaseLevQueue1 n
                      , bench "repeated write some, read some" $ runtestChaseLevQueue2 n
                      ]
                ]
            ]
        , bgroup "Forking, context switches, and Misc. on a single core" $
            [ bench "forkIO" (forkIO (return ()) >> return ())
            , bench "put,take MVar" $ do
                putMVar mv '0'
                takeMVar mv
            , bench "put,takeMVar + forkIO + 2 context switches" $ do
                forkIO $ putMVar mv '0'
                takeMVar mv
            , bench "getNumCapabilities" getNumCapabilities
            , bench "myThreadId" myThreadId

            , bench "myThreadId >>= threadCapability" $ myThreadId >>= threadCapability
            -- It may not be possible to re-use generators for our applications of random, so:
            , bench "random new_gen" $ newStdGen
            , bench "random Int range: (1,8)" $ whnf (randomR (1 :: Int, 8)) sys_rand_gen
          -- THIS IS CRAZY SLOW TODO move out of this so we can actually compare graphs:
          --, bench "mwc-random new_gen" $ createSystemRandom
            , bench "mwc-random Int range: (1,8)" $ ((uniformR (1 :: Int, 8) mwc_gen) :: IO Int)
            , bench "randomRIO (1,8)" $ randomRIO (1::Int,8)

            , bench "old-time getClockTime" $ getClockTime
            , bench "time getPOSIXTime" getPOSIXTime
            ]

        , bgroup "Var primitives" $
            [ bgroup "For scale" $ 
                [ bench "mod" $ nf (2147483647 `mod`) (8 :: Int)
                , bench "rem" $ nf (2147483647 `rem`) (8 :: Int)
                ]
            , bgroup "atomic-primops (puts on safety goggles...)" $ 
                [ bench "newCounter" $ newCounter 0
                , bench "incrCounter 1" $ incrCounter 1 atomic_counter
                , bench "incrCounter 1024" $ incrCounter 1024 atomic_counter
                ]
            , bgroup "IORef" $ 
                [ bench "newIORef Char" $ (newIORef '0')
                , bench "writeIORef" $ (writeIORef ior '1')
                , bench "readIORef" $ (readIORef ior)
                , bench "readIORef (w result forced)" $ nfIO (readIORef ior) -- what does this tell us w/r/t above?
                , bench "readForCAS" $ (fmap peekTicket $ readForCAS ior)
                , bench "readForCAS (w result forced)" $ nfIO (fmap peekTicket $ readForCAS ior)
                , bench "modifyIORef' (i.e. readIORef + writeIORef)" (modifyIORef' ior $ const '2')
                , bench "atomicModifyIORef' (i.e. (in GHC) strict atomicModifyMutVar)" $ (atomicModifyIORef' ior $ const ('3','3'))
                , bench "atomicModifyIORef (i.e. (in GHC) atomicModifyMutVar)" $ (atomicModifyIORef ior $ const ('3','3'))
                
                , bench "atomicModifyIORefCAS (i.e. atomic-primops CAS loop)" $ (atomicModifyIORefCAS ior $ const ('4','4'))

                ]
            , bgroup "MVar" $
                [ bench "newEmptyMVar" $ newEmptyMVar
                , bench "newEmptyMVar + putMVar (i.e. newMVar Char)" (newEmptyMVar >>= \v->putMVar v '0')
                , bench "putMVar + takeMVar" $ (putMVar mv '1' >> takeMVar mv)
                , bench "modifyMVarMasked_ (i.e. mask + takeMVar + putMVar + exception-handling)" $ (modifyMVarMasked_ mvFull (const $ return '2'))
                , bench "mask_ + takeMVar-and-mod + putMVar)" $ mask_ (takeMVar mvFull >>= (putMVar mvFull . const '2'))
                , bench "newMVar + mkWeakMVar with finalizer" $ (newMVar '1' >>= flip mkWeakMVar (return ()))
                -- These show no effect of a finalizer:
                -- , bench "on MVar with finalizer: putMVar, takeMVar" $ (putMVar mvWithFinalizer '1' >> takeMVar mvWithFinalizer)
                -- , bench "On target of an MVar finalizer: takeMVar, putMVar" $ (takeMVar mvFinalizee >>= putMVar mvFinalizee)
#if MIN_VERSION_base(4,7,0)
                , bench "tryReadMVar" $ tryReadMVar mvFull
                , bench "tryReadMVar (w result forced)" $ nfIO $ tryReadMVar mvFull
#endif
                ]

            , bgroup "TVar" $
                [ bench "newTVarIO ()" $ (newTVarIO ())
                , bench "atomically writeTVar" $ (atomically $ writeTVar tv '1')
                , bench "atomically readTVar" $ (atomically $ readTVar tv)
                , bench "readTVarIO" $ (readTVarIO tv)
                , bench "atomically modifyTVar' (i.e. atomically (readTVar + writeTVar))" $ (atomically $ modifyTVar' tv (const '2'))
                , bench "(atomically writeTVar) + (atomically readTVar)" $ ((atomically $ writeTVar tv '1') >> (atomically $ readTVar tv))
                ]
            , bgroup "TMVar" $
                [ bench "newEmptyTMVar (i.e. newTVarIO)" $ newEmptyTMVarIO
                , bench "atomically (newEmptyTMVar + putTMVar) (i.e. newTVar + readTVar + writeTVar,retry)" (atomically (newEmptyTMVar >>= \v->putTMVar v ()))
                , bench "atomically (putTMVar + takeTMVar)" $ (atomically $ (putTMVar tmv '1' >> takeTMVar tmv))
                , bench "(atomically putTMVar) + (atomically takeTMVar)" $ ((atomically $ putTMVar tmv '1') >> (atomically $ takeTMVar tmv))
                ]
            , bgroup "MutVar" $
                [ bench "newMutVar ()" $ (newMutVar () :: IO (MutVar (PrimState IO) ()))
                , bench "writeMutVar" $ (writeMutVar mutv '1' :: IO ())
                , bench "readMutVar" $ (readMutVar mutv :: IO Char)
                , bench "atomicModifyMutVar" $ (atomicModifyMutVar mutv $ const ('2','2') :: IO Char)
                ]
            ]
        -- Some more tests of pure operations relevant to TQueue style dequeue
        -- performance.
        , bgroup "Misc" $
            -- of interest for TQueue style approach
            [ bgroup "cons and reverse" $ 
                [ bench "cons" $ nf (:[]) True
                , bench "pure unrolled cons then final reverse x10" $       nf testConsUnrolledReverse 10
                , bench "pure cons then final reverse x10" $       nf testConsReverse 10
                , bench "pure unrolled cons then final reverse x5" $       nf testConsUnrolledReverse 5
                , bench "pure cons then final reverse x5" $       nf testConsReverse 5

                , bench "pure cons-composition append x10" $ nf testCompositionAppend 10
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
            , bgroup "arrays" $
                [ bgroup "indexing" $ 
                    [ bench "index 8th list" $ nf ([(1::Int)..8] !!) 7
                    , bench "index 8th IArray" $ nf (arr8 !) 7
                    , bench "index 8th Vector" $ nf (V.unsafeIndex vec8) 7
                    , bench "index 8th MVector" $ nfIO $ (MV.unsafeRead mvec8) 7
                    -- I think this is basically MVector AFAICT
                    , bench "index 8th Primitiv MutableArray" $ nfIO $ (P.readArray parr8) 7
                    , bench "index 8th Primitiv Array" $ nf (P.indexArray iparr8) 7

                    , bench "index 16th list" $ nf ([(1::Int)..16] !!) 15
                    , bench "index 16th IArray" $ nf (arr16 !) 15
                    , bench "index 16th Vector" $ nf (V.unsafeIndex vec16) 15
                    , bench "index 16th MVector" $ nfIO $ (MV.unsafeRead mvec16) 15
                    , bench "index 16th Primitiv MutableArray" $ nfIO $ (P.readArray parr16) 15
                    , bench "index 16th Primitiv Array" $ nf (P.indexArray iparr16) 15

                    , bench "readArrayElem for CAS (MutableArray)" $ nfIO (fmap peekTicket $ readArrayElem parr16 15 )
                    , bench "readArray (MutableArray)" $ nfIO (P.readArray parr16 15 )
                    , bench "readByteArray (MutableByteArray, usable for CAS)" $ nfIO (P.readByteArray ba16 15 :: IO Int)
                    , bench "read Mutable Unboxed Vector" $ (UMV.read umvvec16 15 :: IO Int)
                    ]
                , bgroup "writing" $
                    [ bench "write MutableArray" $ (P.writeArray parr16 15 1 :: IO ())
                    , bench "CAS MutableArray (along with a readArrayElem)" (readArrayElem parr16 15 >>= (\t-> casArrayElem parr16 15 t 2))
                    , bench "write MutableByteArray" (P.writeByteArray ba16 15 (1::Int) :: IO ())
                    , bench "CAS MutableByteArray (along with a readByteArray)" (P.readByteArray ba16 15 >>= (\t-> casByteArrayInt ba16 15 t (2::Int)))
                    , bench "write Mutable Unboxed Vector" $ (UMV.write umvvec16 15 2 :: IO ())
                    ]
                , bgroup "creating" $
                    [ bench "new MVector 8 Ints" $ (MVec.new 8 :: IO (MVec.IOVector Int))
                    , bench "new MVector 32 Ints" $ (MVec.new 32 :: IO (MVec.IOVector Int))
                    , bench "unsafeNew MVector 8 Ints" $ (MVec.unsafeNew 8 :: IO (MVec.IOVector Int))
                    , bench "unsafeNew MVector 32 Ints" $ (MVec.unsafeNew 32 :: IO (MVec.IOVector Int))
                    , bench "new MutableArray 8 Ints" $ (P.newArray 8 0 :: IO (P.MutableArray (PrimState IO) Int))
                    , bench "new MutableArray 32 Ints" $ (P.newArray 32 0 :: IO (P.MutableArray (PrimState IO) Int))
                    , bench "new MutableArray 32 Nothing :: Maybe Ints" $ (P.newArray 32 Nothing :: IO (P.MutableArray (PrimState IO) (Maybe Int)))
                    , bench "new MutableByteArray 8 Ints" (P.newByteArray (8* P.sizeOf (0 :: Int)) :: IO (P.MutableByteArray (PrimState IO)))
                    , bench "new MutableByteArray 32 Ints" (P.newByteArray (32* P.sizeOf (0 :: Int)) :: IO (P.MutableByteArray (PrimState IO)))
                    , bench "new MutableByteArray 128 Ints" (P.newByteArray (128* P.sizeOf (0 :: Int)) :: IO (P.MutableByteArray (PrimState IO)))
                    , bench "new set MutableByteArray 128 Ints" $ (P.newByteArray (128* P.sizeOf (0 :: Int)) :: IO (P.MutableByteArray (PrimState IO))) >>= \a-> P.setByteArray a 0 128 (0 :: Int)
                    , bench "new unboxed Mutable Vector 8 Ints" (UMV.new 8 :: IO (UMV.IOVector Int))
                    , bench "new unboxed Mutable Vector 32 Ints" (UMV.new 32 :: IO (UMV.IOVector Int))
                    , bench "new unboxed Mutable Vector 128 Ints" (UMV.new 128 :: IO (UMV.IOVector Int))
                    ]
                ]
            ]
        ]
  -- takeMVar mvWithFinalizer -- keep finalizer from actually running


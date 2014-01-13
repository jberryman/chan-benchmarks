Criterion benchmarks for the different haskell concurrent channel
implementations in `base` and `stm` and elsewhere, as well as simple var
read/write benchmarks for `MVar`, `IORef`, and `TVar` and others.

These benchmarks were originally taken from bench/chanbench.hs in the `stm`
package, ported to criterion with some additions. To run them on your machine:

    cabal sandbox init
    cabal install
    # For HTML reports:
    ./.cabal-sandbox/bin/chan-benchmarks -g -o Benchmarks.html +RTS -N

Feel free to send pull requests with new or improved benchmarks.

# Sample Results

Nice HTML output for a sample run performed on:

    $ lscpu 
    Architecture:          i686
    CPU op-mode(s):        32-bit, 64-bit
    Byte Order:            Little Endian
    CPU(s):                4
    On-line CPU(s) list:   0-3
    Thread(s) per core:    2
    Core(s) per socket:    2
    Socket(s):             1
    Vendor ID:             GenuineIntel
    CPU family:            6
    Model:                 58
    Stepping:              9
    CPU MHz:               1400.000
    BogoMIPS:              4988.38
    Virtualization:        VT-x
    L1d cache:             32K
    L1i cache:             32K
    L2 cache:              256K
    L3 cache:              4096K


...are at `Benchmarks.chans_sample.html` and `Benchmarks.vars_sample.html`.

## Some analysis of primitive operations

    forkIO           309.ns
    context switch  2975.ns

    getNumCapabilities    4.1ns
    myThreadId            4.7ns

    newIORef      7.19
    readIORef     3.74ns
    writeIORef    7.02ns
    modifyIORef'  7.02ns  -- even though this is implemented as read+write??
    atomicModifyIORef'   22.43ns
    atomicModifyIORef    53.67ns  -- variable; showing cost of lazy creation of (const 'x') thunks?

    newEmptyMVar  7.32ns
    takeMVar     16.21ns
    putMVar       9.02ns 
    modifyMVarMasked_   35.09ns  -- handler overhead ~ 10ns

    newTVarIO    12.96ns
    atomically writeTVar    53.35ns
    atomically readTVar     54.29ns
    readTVarIO               4.13ns
    atomically modifyTVar'  63.76ns

## Random resources

Some discussion of nitty-gritty of `atomicModifyIORef`:

    http://stackoverflow.com/questions/10102881/haskell-how-does-atomicmodifyioref-work
    

## Random Analysis

Back-of-envelope look at how primitive var read write cost relates to chan RW
cost.

As of the current test run, looking at the mean times for the fastest three
contenders on the easiest test (write some, read some), we get the following
mean timings for *one read and write* (although reads and writes might vary
widely)

    Chan             135 ns
    TQueue           175 ns
    chan-split-fast   88 ns

Measured timings for an atomic `modify` (or take/put; again not ideal) divided
by 2 (i.e. very approx timing for a take/read or put/write):

    MVar    15 ns
    TVar    36 ns

and var creation:

    MVar    24 ns
    TVar    21 ns

Counting var operations, with (count) around slowest path

    chan-split-fast
    ---------------
                 puts  takes  creates   TOTAL
    readChan     1(2)  1(3)   (1)       30-99 ns
    writeChan    1(2)  1      /         30-45 ns
              TOTAL for read and write: 60-144 ns

So if we didn't screw that up:
    - yes, read/write timing dominates
    - but there might still be room to shave time elsewhere
    - in "write some / read some" we don't take the slow reader-blocked path much, as expected

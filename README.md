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

Criterion benchmarks for the different haskell concurrent channel
implementations in `base` and `stm`.

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


...are at `Benchmarks.sample.html` for the following benchmark run of 
`$ chan-benchmarks -g -o Benchmarks.html +RTS -N`:

    warming up
    estimating clock resolution...
    mean is 2.277726 us (320001 iterations)
    found 1965 outliers among 319999 samples (0.6%)
      1373 (0.4%) high severe
    estimating cost of a clock call...
    mean is 128.0560 ns (22 iterations)
    found 3 outliers among 22 samples (13.6%)
      2 (9.1%) high mild
      1 (4.5%) high severe

    benchmarking Chan/async 1 writer 1 reader
    mean: 29.46111 ms, lb 29.24964 ms, ub 29.67216 ms, ci 0.950
    std dev: 1.080033 ms, lb 988.9366 us, ub 1.184190 ms, ci 0.950
    variance introduced by outliers: 33.576%
    variance is moderately inflated by outliers

    benchmarking Chan/sequential write all then read all
    mean: 28.69876 ms, lb 28.68136 ms, ub 28.73306 ms, ci 0.950
    std dev: 119.7542 us, lb 74.73703 us, ub 229.9646 us, ci 0.950

    benchmarking Chan/repeated write some, read some
    mean: 13.47483 ms, lb 13.46907 ms, ub 13.48312 ms, ci 0.950
    std dev: 34.91742 us, lb 26.49370 us, ub 45.43417 us, ci 0.950

    benchmarking Chan/async 2 writers two readers
    mean: 29.88702 ms, lb 29.25368 ms, ub 30.57884 ms, ci 0.950
    std dev: 3.396463 ms, lb 3.006652 ms, ub 3.851317 ms, ci 0.950
    variance introduced by outliers: 83.146%
    variance is severely inflated by outliers

    benchmarking Chan/async 3 writers 1 reader
    mean: 28.39919 ms, lb 28.04733 ms, ub 28.90150 ms, ci 0.950
    std dev: 2.129175 ms, lb 1.584765 ms, ub 2.743697 ms, ci 0.950
    found 9 outliers among 100 samples (9.0%)
      9 (9.0%) high severe
    variance introduced by outliers: 67.677%
    variance is severely inflated by outliers

    benchmarking TChan/async 1 writer 1 reader
    mean: 43.22383 ms, lb 42.87391 ms, ub 43.54337 ms, ci 0.950
    std dev: 1.722101 ms, lb 1.509122 ms, ub 1.961421 ms, ci 0.950
    found 5 outliers among 100 samples (5.0%)
      5 (5.0%) low mild
    variance introduced by outliers: 36.579%
    variance is moderately inflated by outliers

    benchmarking TChan/sequential write all then read all
    collecting 100 samples, 1 iterations each, in estimated 5.296206 s
    mean: 50.27657 ms, lb 50.23123 ms, ub 50.36707 ms, ci 0.950
    std dev: 315.3303 us, lb 188.4642 us, ub 620.7174 us, ci 0.950

    benchmarking TChan/repeated write some, read some
    collecting 100 samples, 1 iterations each, in estimated 6.592798 s
    mean: 25.91603 ms, lb 25.90330 ms, ub 25.95407 ms, ci 0.950
    std dev: 103.1572 us, lb 43.84586 us, ub 224.9942 us, ci 0.950

    benchmarking TChan/async 2 writers two readers
    mean: 44.27526 ms, lb 43.72635 ms, ub 44.79635 ms, ci 0.950
    std dev: 2.734358 ms, lb 2.419183 ms, ub 3.099398 ms, ci 0.950
    variance introduced by outliers: 58.525%
    variance is severely inflated by outliers

    benchmarking TChan/async 3 writers 1 reader
    mean: 43.77147 ms, lb 43.22038 ms, ub 44.41441 ms, ci 0.950
    std dev: 3.055731 ms, lb 2.605042 ms, ub 3.482208 ms, ci 0.950
    found 17 outliers among 100 samples (17.0%)
      16 (16.0%) high mild
      1 (1.0%) high severe
    variance introduced by outliers: 64.603%
    variance is severely inflated by outliers

    benchmarking TQueue/async 1 writer 1 reader
    mean: 24.28301 ms, lb 24.12991 ms, ub 24.41706 ms, ci 0.950
    std dev: 734.1300 us, lb 642.8480 us, ub 835.1657 us, ci 0.950
    variance introduced by outliers: 24.844%
    variance is moderately inflated by outliers

    benchmarking TQueue/sequential write all then read all
    mean: 23.47465 ms, lb 23.46321 ms, ub 23.49287 ms, ci 0.950
    std dev: 72.26607 us, lb 52.32564 us, ub 118.3833 us, ci 0.950

    benchmarking TQueue/repeated write some, read some
    mean: 17.21998 ms, lb 17.21182 ms, ub 17.23094 ms, ci 0.950
    std dev: 48.33131 us, lb 38.88651 us, ub 62.58085 us, ci 0.950

    benchmarking TQueue/async 2 writers two readers
    mean: 21.77323 ms, lb 21.63715 ms, ub 21.94799 ms, ci 0.950
    std dev: 786.6068 us, lb 625.3728 us, ub 959.3007 us, ci 0.950
    found 14 outliers among 100 samples (14.0%)
      2 (2.0%) low mild
      11 (11.0%) high severe
    variance introduced by outliers: 32.616%
    variance is moderately inflated by outliers

    benchmarking TQueue/async 3 writers 1 reader
    mean: 23.71692 ms, lb 23.47976 ms, ub 24.07713 ms, ci 0.950
    std dev: 1.478231 ms, lb 1.048440 ms, ub 2.036562 ms, ci 0.950
    found 21 outliers among 100 samples (21.0%)
      2 (2.0%) high mild
      18 (18.0%) high severe
    variance introduced by outliers: 59.503%
    variance is severely inflated by outliers

    benchmarking TBQueue/async 1 writer 1 reader
    mean: 35.37391 ms, lb 35.33321 ms, ub 35.43962 ms, ci 0.950
    std dev: 258.2778 us, lb 174.9580 us, ub 422.8317 us, ci 0.950

    benchmarking TBQueue/sequential write all then read all
    mean: 40.98833 ms, lb 40.97178 ms, ub 41.00412 ms, ci 0.950
    std dev: 82.35813 us, lb 67.75618 us, ub 105.1711 us, ci 0.950

    benchmarking TBQueue/repeated write some, read some
    mean: 33.32238 ms, lb 33.29651 ms, ub 33.42212 ms, ci 0.950
    std dev: 227.6349 us, lb 47.78914 us, ub 526.8011 us, ci 0.950

    benchmarking TBQueue/async 2 writers two readers
    mean: 36.89353 ms, lb 36.86225 ms, ub 36.94133 ms, ci 0.950
    std dev: 195.2013 us, lb 137.2115 us, ub 286.7888 us, ci 0.950

    benchmarking TBQueue/async 3 writers 1 reader
    mean: 36.78935 ms, lb 36.75279 ms, ub 36.82488 ms, ci 0.950
    std dev: 185.9522 us, lb 163.1437 us, ub 213.9863 us, ci 0.950

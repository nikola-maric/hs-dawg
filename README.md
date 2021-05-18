# hs-dawg

My attempt to implement [DAFSA](https://en.wikipedia.org/wiki/Deterministic_acyclic_finite_state_automaton) using haskell. Logic is inspired by paper “Incremental Construction of Minimal Acyclic Finite-State Automata”, by Jan Daciuk, Stoyan Mihov, Bruce W. Watson, and Richard E. Watson. Published in 2000 in Computational Linguistics 26(1), pp.3-16. Available online at https://www.aclweb.org/anthology/J00-1002.

Also, some parts of code take inspiration from https://github.com/antalsz/text-set, although I changed some parts of it (mostly the way final graph looks like and how equivalence of children is performed). But code in the above link is much prettier in my opinion and worth checking out, I learned a lot from it.

### Important note

Built just for fun, see benchmark results at the end.

### Implementation bits

Final graph contains "transitions", that is, every Map entry is a :
- key is nodeId + termination bit + 32 bits taken from character
- value is nodeId + termination bit of node  that comes when we go from node contained in key, using character provided

If we exaust all letters of string that we are searching for and we end up with Int that has termination bit set, we are done and graph contains that string. In all other cases String is not contained (either there is no nodeId + char transition or we exausted imput string and we got to non terminating node info).

So this kinda works only on machines with 64 bit Int-s, and with graphs that do not exceed 2^31 members :) 

### Performance

Some choices look weird (like using both `Data.Map.Strict` and `Data.HashMap.Strict`), but I found performance improvements when doing some thigs using one and other things using the other one. 

One of the weirder ones was that code was faster when I'm using `graphTransitions :: !(M.Map Int Int)` instead of `graphTransitions :: !IntMap Int`. I was under impression that IntMap would prove more performant, as it was specialized for Ints? 

These are benchmark results, comparing this library, text-set one and regular Data.Set:

```
benchmarking construction/Construction 10.000 hs-dawg
time                 296.4 ms   (279.1 ms .. 308.6 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 309.7 ms   (303.6 ms .. 315.1 ms)
std dev              7.263 ms   (4.535 ms .. 10.89 ms)
variance introduced by outliers: 16% (moderately inflated)
                     
benchmarking construction/Construction 10.000 Set
time                 1.586 ms   (1.511 ms .. 1.668 ms)
                     0.979 R²   (0.965 R² .. 0.989 R²)
mean                 1.650 ms   (1.599 ms .. 1.713 ms)
std dev              191.3 μs   (149.5 μs .. 238.2 μs)
variance introduced by outliers: 76% (severely inflated)
                     
benchmarking construction/Construction 10.000 TextSet
time                 3.372 s    (1.840 s .. 4.806 s)
                     0.975 R²   (0.909 R² .. 1.000 R²)
mean                 3.329 s    (3.070 s .. 3.500 s)
std dev              259.6 ms   (122.7 ms .. 340.0 ms)
variance introduced by outliers: 21% (moderately inflated)
                     
benchmarking lookup hs-dawg/10.000
time                 635.8 ns   (624.1 ns .. 648.2 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 630.9 ns   (626.5 ns .. 636.8 ns)
std dev              17.57 ns   (12.11 ns .. 24.65 ns)
variance introduced by outliers: 39% (moderately inflated)
                     
benchmarking lookup Set/10.000
time                 199.7 ns   (196.3 ns .. 204.6 ns)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 201.0 ns   (199.0 ns .. 203.9 ns)
std dev              8.145 ns   (5.802 ns .. 13.36 ns)
variance introduced by outliers: 60% (severely inflated)
                     
benchmarking lookup TextSet/10.000
time                 860.5 ns   (847.7 ns .. 874.1 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 877.9 ns   (867.9 ns .. 888.9 ns)
std dev              35.93 ns   (29.65 ns .. 44.62 ns)
variance introduced by outliers: 57% (severely inflated)
```

It is bit faster than TextSet in lookups, and a lot faster in construction, but regular Set wipes the floor with both :D 

Memory benchmarks:
```
Construction Benchmark
                     
  Case       Allocated         Max       Live  GCs       MaxOS
  hs-dawg  547,235,416  19,127,792  6,244,296  526  41,943,040
  Set        1,039,256     400,456    400,456    0           0
  TextSet  225,611,680  34,277,032  8,060,976  188  76,546,048
  ```
Lot more GCs than TextSet, lot more allocations, but in the end less maximum memory in use by the RTS. Agan, Data.Set laughs and continues to drink its beer.

There is simplifer output in this repo for `Graph` module in [link](https://github.com/nikola-maric/hs-dawg/blob/master/src/DAFSA/Graph.dump-simpl) and I suspect it could be made faster if I reduce number of packing/unpacking in `contains` method, but when I tried I didnt get anywhere.

From memory side, most of allocations happen in `addNodeToGraph`/`replaceOrRegister`/`swapChildState`. Im including ouptuts of running app with `stack --profile run hs-dawg-exe --rts-options -p` and also output of [profiteur tool](https://hackage.haskell.org/package/profiteur) [here](https://github.com/nikola-maric/nikola-maric.github.io/tree/master/hs-dawg-perf)




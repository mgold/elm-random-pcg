# Benchmarks for Random.PCG

These benchmarks are possible thanks to [JoeyEremondi/elm-benchmark](https://github.com/JoeyEremondi/elm-benchmark). To
run them yourself, run `elm-reactor` in this directory and open `Test.elm`.

For generating floats, there is no measurable difference between core/Random and PCG. For generating integers, PCG has a
"fast path" when the range is a power of two. For these ranges, PCG is almost as fast as core/Random. For other ranges,
it is between two-thirds and one-half the speed of core/Random. But as my college prof used to say, there's no prize for
getting wrong answers quickly...

Here is one set of results:
```
Core: flip a coin x 768,348 ops/sec ±1.09% (91 runs sampled)
PCG:  flip a coin x 675,647 ops/sec ±1.19% (94 runs sampled)
Core: flip 1000 coins x 635 ops/sec ±1.55% (89 runs sampled)
PCG:  flip 1000 coins x 612 ops/sec ±0.75% (90 runs sampled)
Core: generate an integer 0-4094 x 723,676 ops/sec ±1.93% (86 runs sampled)
PCG:  generate an integer 0-4094 x 444,396 ops/sec ±0.94% (89 runs sampled)
Core: generate an integer 0-4095 x 735,939 ops/sec ±0.98% (92 runs sampled)
PCG:  generate an integer 0-4095 x 678,857 ops/sec ±0.95% (88 runs sampled)
Core: generate an integer 0-4096 x 793,462 ops/sec ±0.69% (90 runs sampled)
PCG:  generate an integer 0-4096 x 426,862 ops/sec ±0.84% (91 runs sampled)
Core: generate a massive integer x 722,485 ops/sec ±1.23% (91 runs sampled)
PCG:  generate a massive integer x 715,050 ops/sec ±0.75% (89 runs sampled)
Core: generate a percentage x 396,709 ops/sec ±0.81% (92 runs sampled)
PCG:  generate a percentage x 390,411 ops/sec ±0.83% (90 runs sampled)
Core: generate 1000 percentages x 350 ops/sec ±1.20% (86 runs sampled)
PCG:  generate 1000 percentages x 351 ops/sec ±0.88% (86 runs sampled)
Core: generate a float 0-4094 x 357,585 ops/sec ±0.82% (91 runs sampled)
PCG:  generate a float 0-4094 x 389,361 ops/sec ±0.82% (90 runs sampled)
Core: generate a float 0-4095 x 397,307 ops/sec ±1.47% (87 runs sampled)
PCG:  generate a float 0-4095 x 399,135 ops/sec ±0.75% (93 runs sampled)
Core: generate a float 0-4096 x 394,608 ops/sec ±0.91% (92 runs sampled)
PCG:  generate a float 0-4096 x 396,503 ops/sec ±0.83% (89 runs sampled)
Core: generate a massive float x 391,631 ops/sec ±1.07% (90 runs sampled)
PCG:  generate a massive float x 401,398 ops/sec ±0.86% (93 runs sampled)
```

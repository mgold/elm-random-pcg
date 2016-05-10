# Benchmarks for Random.Pcg

These benchmarks are possible thanks to [JoeyEremondi/elm-benchmark](https://github.com/JoeyEremondi/elm-benchmark). To
run them yourself, run `elm-reactor` in this directory and open `Test.elm`.

For generating floats, there is no measurable difference between core/Random and Pcg. For generating integers, Pcg has a
"fast path" when the range is a power of two. For these ranges, Pcg is almost as fast as core/Random. For other ranges,
it is between two-thirds and one-half the speed of core/Random. But as my college prof used to say, there's no prize for
getting wrong answers quickly...

Here is one set of results:
```
Core: flip a coin x 691,680 ops/sec ±1.44% (91 runs sampled)
Pcg:  flip a coin x 609,211 ops/sec ±0.77% (95 runs sampled)
Core: flip 1000 coins x 643 ops/sec ±1.03% (91 runs sampled)
Pcg:  flip 1000 coins x 569 ops/sec ±0.82% (91 runs sampled)
Core: generate an integer 0-4094 x 728,934 ops/sec ±1.60% (88 runs sampled)
Pcg:  generate an integer 0-4094 x 432,310 ops/sec ±1.17% (93 runs sampled)
Core: generate an integer 0-4095 x 773,749 ops/sec ±0.90% (92 runs sampled)
Pcg:  generate an integer 0-4095 x 654,965 ops/sec ±0.88% (92 runs sampled)
Core: generate an integer 0-4096 x 751,325 ops/sec ±0.86% (90 runs sampled)
Pcg:  generate an integer 0-4096 x 415,061 ops/sec ±1.59% (86 runs sampled)
Core: generate a massive integer x 654,765 ops/sec ±0.95% (94 runs sampled)
Pcg:  generate a massive integer x 627,977 ops/sec ±0.76% (94 runs sampled)
Core: generate a percentage x 537,604 ops/sec ±0.58% (97 runs sampled)
Pcg:  generate a percentage x 356,740 ops/sec ±0.78% (93 runs sampled)
Core: generate 1000 percentages x 502 ops/sec ±0.93% (91 runs sampled)
Pcg:  generate 1000 percentages x 345 ops/sec ±0.70% (88 runs sampled)
Core: generate a float 0-4094 x 509,932 ops/sec ±0.76% (95 runs sampled)
Pcg:  generate a float 0-4094 x 355,706 ops/sec ±1.01% (92 runs sampled)
Core: generate a float 0-4095 x 515,818 ops/sec ±0.69% (93 runs sampled)
Pcg:  generate a float 0-4095 x 357,914 ops/sec ±0.64% (94 runs sampled)
Core: generate a float 0-4096 x 539,042 ops/sec ±0.94% (95 runs sampled)
Pcg:  generate a float 0-4096 x 356,700 ops/sec ±0.89% (95 runs sampled)
Core: generate a massive float x 547,601 ops/sec ±0.85% (90 runs sampled)
Pcg:  generate a massive float x 365,904 ops/sec ±0.67% (95 runs sampled)
Pcg:  generate an independent (split) seed x 95,798 ops/sec ±0.82% (95 runs sampled)
Pcg:  fast forward a seed 1 time x 182,741 ops/sec ±0.82% (95 runs sampled)
Pcg:  fast forward a seed 10 times x 76,553 ops/sec ±0.69% (95 runs sampled)
Pcg:  fast forward a seed 100 times x 47,789 ops/sec ±0.55% (93 runs sampled)
Pcg:  fast forward a seed 1000 times x 30,348 ops/sec ±0.70% (92 runs sampled)
Pcg:  fast forward a seed 1 million times x 18,304 ops/sec ±1.20% (94 runs sampled)
```

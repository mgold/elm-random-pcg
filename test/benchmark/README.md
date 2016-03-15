# Benchmarks for Random.PCG

These benchmarks are possible thanks to [JoeyEremondi/elm-benchmark](https://github.com/JoeyEremondi/elm-benchmark). To
run them yourself, run `elm-reactor` in this directory and open `Test.elm`.

For generating floats, there is no measurable difference between core/Random and PCG. For generating integers, PCG has a
"fast path" when the range is a power of two. For these ranges, PCG is almost as fast as core/Random. For other ranges,
it is between two-thirds and one-half the speed of core/Random. But as my college prof used to say, there's no prize for
getting wrong answers quickly...

Here is one set of results:
```
Core: flip a coin x 718,773 ops/sec ±1.22% (89 runs sampled)
PCG:  flip a coin x 706,727 ops/sec ±1.05% (92 runs sampled)
Core: flip 1000 coins x 690 ops/sec ±1.54% (88 runs sampled)
PCG:  flip 1000 coins x 668 ops/sec ±0.89% (90 runs sampled)
Core: generate an integer 0-4094 x 879,377 ops/sec ±1.89% (86 runs sampled)
PCG:  generate an integer 0-4094 x 509,113 ops/sec ±0.81% (94 runs sampled)
Core: generate an integer 0-4095 x 895,087 ops/sec ±1.27% (92 runs sampled)
PCG:  generate an integer 0-4095 x 741,361 ops/sec ±0.78% (92 runs sampled)
Core: generate an integer 0-4096 x 868,041 ops/sec ±0.99% (92 runs sampled)
PCG:  generate an integer 0-4096 x 494,853 ops/sec ±1.22% (93 runs sampled)
Core: generate a massive integer x 741,193 ops/sec ±1.05% (94 runs sampled)
PCG:  generate a massive integer x 751,794 ops/sec ±0.77% (94 runs sampled)
Core: generate a percentage x 415,547 ops/sec ±0.71% (96 runs sampled)
PCG:  generate a percentage x 428,825 ops/sec ±1.00% (94 runs sampled)
Core: generate 1000 percentages x 411 ops/sec ±0.84% (92 runs sampled)
PCG:  generate 1000 percentages x 417 ops/sec ±0.76% (90 runs sampled)
Core: generate a float 0-4094 x 405,720 ops/sec ±0.83% (95 runs sampled)
PCG:  generate a float 0-4094 x 434,653 ops/sec ±1.06% (91 runs sampled)
Core: generate a float 0-4095 x 433,676 ops/sec ±1.17% (92 runs sampled)
PCG:  generate a float 0-4095 x 440,822 ops/sec ±0.66% (94 runs sampled)
Core: generate a float 0-4096 x 431,486 ops/sec ±0.72% (92 runs sampled)
PCG:  generate a float 0-4096 x 438,342 ops/sec ±1.00% (93 runs sampled)
Core: generate a massive float x 419,978 ops/sec ±0.62% (94 runs sampled)
PCG:  generate a massive float x 440,922 ops/sec ±1.03% (96 runs sampled)
PCG:  split a seed x 108,365 ops/sec ±0.81% (93 runs sampled)
PCG:  fast forward a seed 1 time x 220,689 ops/sec ±0.91% (93 runs sampled)
PCG:  fast forward a seed 10 times x 82,846 ops/sec ±0.81% (93 runs sampled)
PCG:  fast forward a seed 100 times x 50,294 ops/sec ±0.69% (95 runs sampled)
PCG:  fast forward a seed 1000 times x 35,102 ops/sec ±1.27% (92 runs sampled)
PCG:  fast forward a seed 1 million times x 21,259 ops/sec ±0.84% (95 runs sampled)
```

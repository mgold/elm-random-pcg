# Random.Pcg for Elm

> "The generation of random numbers is too important to be left to chance." – Robert R. Coveyou

An alternate random number generator built around four principles:

* **Statistical Quality.** If you use any seed less than 53668 and generate one bool, it will be `True` – if you're
using the core library. This library produces far less predictable output, especially if you use thousands of random
numbers. The core library fails after as little as five seconds of scrutiny. Would you want to trust the accuracy of
your [fuzz tests](http://package.elm-lang.org/packages/elm-community/elm-test/latest/) to such a flawed algorithm? See
`test/dieharder` for more details.

* **Useful features.** This library exports `constant` and `andMap`, which are conspicuously absent from core, along with
other helpful functions for composing generators. Especially interesting is the `independentSeed` function which allows
for lazy lists and isolated components to generate as much randomness as they need, when they need it. There is also a
`fastForward` function that allows random access (rather than sequential access) into, effectively, an infinite array of
random numbers. This can be used instead of reading a finite amount of random values into a large data structure.

* **Performace.** This library will generate floats about 3.5 times faster than core, and ints do not regress. These
figures stand to improve pending some optimizations to the compiler. You can see the [full
benchmark results](https://github.com/mgold/elm-random-pcg/issues/5#issuecomment-236398261).

* **Compatibility.** This library is a drop-in replacement forcore's Random module. Specifically, you
can replace `import Random` with `import Random.Pcg as Random` and everything will continue to work. The exception is third party
libraries like [elm-random-extra](http://package.elm-lang.org/packages/NoRedInk/elm-random-extra/latest/Random-Extra));
the long-term resolution is to pull this library into core.

This is an implementation of [PCG](http://www.pcg-random.org/) by M. E. O'Neil. The generator is **not cryptographically
secure**.

Please report bugs, feature requests, and other issues [on GitHub](https://github.com/mgold/elm-random-pcg/issues/new).

## Changelog (major versions only)
### 3.0.0
* Change implementation to use the RXS-M-SH variant of PCG. Now much faster and not much worse statistically.
* Remove `initialSeed2`, since there are now only 32 bits of state.
* `Random.Pcg.Interop.fission` has been changed to a (core) generator of (PCG) seeds.
* Add `generate` to match core 4.x API. Implemented by Richard Feldman.

### 2.0.0
* Upgraded for 0.17.
* `generate` renamed `step` to match core 4.x API.
* Module renamed `Random.Pcg` from `Random.PCG`.
* `split` has been removed; use `independentSeed`.
* `minInt` and `maxInt` values changed to match core.

# Random.PCG for Elm

An alternate random number generator. You can `import Random.PCG as Random` and everything will continue to
work (except [elm-random-extra](http://package.elm-lang.org/packages/NoRedInk/elm-random-extra/2.1.1/Random-Extra)).
This library offers two improvements over core, without a noticeable loss of performance.

* **Better statistical properties.** If you use any seed less than 53668 and generate one bool, it will be `True` – if
you're using the core library. This library produces far less predictable output, especially if you use thousands of
random numbers. The core library fails after as little as five seconds of scrutiny. See `test/dieharder` for more details.

* **More features.** Unlike core, this library exposes a `split` function to create two independent seeds from one. This
allows for lazy lists and isolated components to generate as much randomness as they need, when they need it. There is
also a `fastForward` function that allows random access (rather than sequential access) into, effectively, an infinite
array of random numbers. This can be used instead of reading a finite amount of random values into a large data
structure.

This is an implementation of [PCG](http://www.pcg-random.org/) generators by M. E. O'Neil, based on the [JS
port](https://github.com/thomcc/pcg-random) by Thom Chiovoloni (MIT license). The generator is **not cryptographically
secure**.

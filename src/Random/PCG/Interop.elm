module Random.PCG.Interop where

{-| Provides a method to create a PCG seed from a seed in the core library. This
is useful for library writers who need a splittable PRNG but don't want to
require client code to use the PCG implementation.

```elm
import Random.PCG
import Random.PCG.Interop as Random.PCG
```

@docs fission
-}

import Random
import Random.PCG

{-| Use the core library's random seed to produce a PCG random seed.

-}
fission : Random.Seed -> (Random.PCG.Seed, Random.Seed)
fission stdSeed0 =
  let
    gen = Random.int 0 0xFFFFFFFF
    (a, stdSeed1) = Random.generate gen stdSeed0
    (b, stdSeed2) = Random.generate gen stdSeed1
    pcgSeed1 = Random.PCG.initialSeed2 a b
  in
    (pcgSeed1, stdSeed2)


/* Random.PCG for Elm
 * Max Goldstein
 *
 * PCG by M. E. O'Neil: http://www.pcg-random.org/
 * JS port by Thom Chiovoloni (MIT license): https://github.com/thomcc/pcg-random
 */

Elm.Native = Elm.Native || {};
Elm.Native.Random = Elm.Native.Random || {};
Elm.Native.Random.PCG = Elm.Native.Random.PCG || {};

Elm.Native.Random.PCG.make = function(localRuntime) {
  'use strict';

  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Random = localRuntime.Native.Random || {};
  localRuntime.Native.Random.PCG = localRuntime.Native.Random.PCG || {};
  if ('values' in localRuntime.Native.Random.PCG) {
    return localRuntime.Native.Random.PCG.values;
  }
  if ('values' in Elm.Native.Random.PCG) {
    return localRuntime.Native.Random.PCG.values = Elm.Native.Random.PCG.values;
  }
  var Tuple2 = Elm.Native.Utils.make(localRuntime).Tuple2;

  // okay, we cannot short-ciruit, so now we define everything
  var defaultIncHi = 0x14057b7e;
  var defaultIncLo = 0xf767814f;

  var nextInc = 1;

  function initialSeed2(seedHi, seedLo){
    var state = new Int32Array([ 0, 0, defaultIncHi, defaultIncLo ]);
    state = next(state)[1];
    add64_(state, state[0], state[1], seedHi>>>0, seedLo>>>0);
    state = next(state)[1];
    return state;
  };

  // shim for Math.imul.
  var imul = Math.imul;
  if (!imul) {
    imul = function(a, b) {
      var ah = (a >>> 16) & 0xffff, al = a & 0xffff;
      var bh = (b >>> 16) & 0xffff, bl = b & 0xffff;
      return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0) | 0);
    };
  }

  // multiply two 64 bit numbers (given in parts), and store the result in `out`.
  function mul64_(out, aHi, aLo, bHi, bLo) {
    var c1 = (aLo >>> 16) * (bLo & 0xffff) >>> 0;
    var c0 = (aLo & 0xffff) * (bLo >>> 16) >>> 0;

    var lo = ((aLo & 0xffff) * (bLo & 0xffff)) >>> 0;
    var hi = ((aLo >>> 16) * (bLo >>> 16)) + ((c0 >>> 16) + (c1 >>> 16)) >>> 0;

    c0 = (c0 << 16) >>> 0;
    lo = (lo + c0) >>> 0;
    if ((lo >>> 0) < (c0 >>> 0)) {
      hi = (hi + 1) >>> 0;
    }

    c1 = (c1 << 16) >>> 0;
    lo = (lo + c1) >>> 0;
    if ((lo >>> 0) < (c1 >>> 0)) {
      hi = (hi + 1) >>> 0;
    }

    hi = (hi + imul(aLo, bHi)) >>> 0;
    hi = (hi + imul(aHi, bLo)) >>> 0;

    out[0] = hi;
    out[1] = lo;
  }

  // add two 64 bit numbers (given in parts), and store the result in `out`.
  function add64_(out, aHi, aLo, bHi, bLo) {
    var hi = (aHi + bHi) >>> 0;
    var lo = (aLo + bLo) >>> 0;
    if ((lo >>> 0) < (aLo >>> 0)) {
      hi = (hi + 1) | 0;
    }
    out[0] = hi;
    out[1] = lo;
  }

  var MUL_HI = 0x5851f42d >>> 0;
  var MUL_LO = 0x4c957f2d >>> 0;

  // Generate a random 32 bit integer, and the new state.
  function next(state) {
    // save current state (what we'll use for this number)
    var oldState = state.slice();
    var oldHi = oldState[0] >>> 0;
    var oldLo = oldState[1] >>> 0;

    state = new Int32Array(4);

    // churn LCG.
    mul64_(state, oldHi, oldLo, MUL_HI, MUL_LO);
    add64_(state, state[0], state[1], oldState[2] >>> 0, oldState[3] >>> 0);

    // get least sig. 32 bits of ((oldstate >> 18) ^ oldstate) >> 27
    var xsHi = oldHi >>> 18;
    var xsLo = ((oldLo >>> 18) | (oldHi << 14)) >>> 0;
    xsHi = (xsHi ^ oldHi) >>> 0;
    xsLo = (xsLo ^ oldLo) >>> 0;
    var xorshifted = ((xsLo >>> 27) | (xsHi << 5)) >>> 0;
    // rotate xorshifted right a random amount, based on the most sig. 5 bits
    // bits of the old state.
    var rot = oldHi >>> 27;
    var rot2 = ((-rot >>> 0) & 31) >>> 0;
    return [ ((xorshifted >>> rot) | (xorshifted << rot2)) >>> 0,
             state];
  };

  /// Get a uniformly distributed 32 bit integer between [0, max).
  function integer(max, state) {
    max = max >>> 0;
    var result;
    if ((max & (max - 1)) === 0) {
      // fast path for power of 2
      result = next(state);
      return [result[0] & (max - 1), result[1]];
    }

    var skew = ((-max >>> 0) % max) >>> 0;
    result = next(state)
    // accounting for bias - the loop executes almost never
    while (result[0] < skew){
      result = next(result[1]);
    }
    return [result[0] % max, result[1]];
  };

  function intt(min, max, state){
    if (min == max){
      return Tuple2(min, state);
    }
    var range = Math.abs(max - min) + 1;
    var result = integer(range, state);
    return Tuple2(result[0] + min, result[1]);
  }

  var BIT_53 = 9007199254740992.0;
  var BIT_27 = 134217728.0;

  function floatt(min, max, state) {
    /// Get a uniformly distributed IEEE-754 double between 0.0 and 1.0
    var result1 = next(state);
    var result2 = next(result1[1]);
    var hi = (result1[0] & 0x03ffffff) * 1.0;
    var lo = (result2[0] & 0x07ffffff) * 1.0;
    var val = ((hi * BIT_27) + lo) / BIT_53;

    // Scale it into our range
    var range = Math.abs(max - min);
    var scaled = val * range + min;
    return Tuple2(scaled, result2[1]);
  };

  function split(state){
    var newState = state.slice();
    newState[2] += nextInc;
    nextInc += 1;
    return Tuple2(next(state)[1], next(newState)[1]);
  }

  return localRuntime.Native.Random.PCG.values = {
    initialSeed2 : F2(initialSeed2),
    intt : F3(intt),
    floatt : F3(floatt),
    split : split
  };
};

# PicoMite CSub Toolkit

CSubs allow MMBasic programs to interact with native C code libraries.

This repository packages everything you need to bolt native ARM code onto MMBasic programs. The
headline demo is a Mandelbrot renderer that drops render time from ~70 s to about 10 s on a PicoMite,
but the same flow works for any performance-critical routine (we include a simple integer `pow()` example).

## Why use CSubs?

The interpreter is convenient, but when you’re number‑crunching in tight loops, you pay for it. A one-time CFunction/CSUB gives you:

-   Native Cortex execution (M0+ for RP2040 or M33 for RP2350).
-   Drop-in replacements for slow BASIC loops. No firmware rebuild required.

For Mandelbrot that means drawing a full 320×320 view in about **10 seconds** instead of **70 seconds**.

## Directory layout

```
cfunc/
    build.sh           → compiles a C source into .elf/.bin
    build_mand.sh         → recompiles mandelbrot_cf.c, patches mand.bas + test_mandel.bas
    build_pow.sh          → recompiles pow_int_cf.c, patches test_pow.bas
    emit_cfunction_block.sh
                        → formats a .bin as a ready-to-paste CSUB block
    pow_int_cf.c       → integer exponent routine (command-style CSUB)
    mandelbrot_cf.c    → Q3.28 fixed-point Mandelbrot worker (with cardioid/bulb checks)
    mand.bas           → BASIC demo with interactive zoom menu using the native CSUB
    test_mandel.bas    → minimal Mandelbrot test harness
    test_pow.bas       → minimal pow() test harness
```

## Requirements

-   ARM bare-metal toolchain (e.g. `brew install arm-none-eabi-gcc` on macOS).
-   `python3` (used by the build helpers).

## Quick start: native power function

```sh
cd cfunc
./build_pow.sh                # builds pow_int_cf.c and updates test_pow.bas
```

`test_pow.bas` now contains the CSUB block plus the boilerplate BASIC code that calls it:

```basic
pow_int r%, base%, exp%   ' command-style call writes result back
Print r%
```

Swap in your own BASIC code, or copy the CSUB block to another program.

## Mandelbrot example (10 s render)

```sh
cd cfunc
./build_mand.sh              # rebuilds mandelbrot_cf.c and patches mand.bas & test_mandel.bas
```

`mand.bas` now contains:

-   The Mandelbrot CSUB block.
-   A palette and renderer that call the CSUB in 64-pixel chunks.
-   A zoom menu (`Z` to highlight a square and zoom, `O` to back out, `R` to reset).

Load it on the PicoMite and run—full-screen render in ~10 seconds, compared to ~70 seconds for the
pure BASIC version.

## Targeting RP2040 vs RP2350

`build.sh` defaults to the RP2350’s Cortex-M33 core. To compile for an RP2040 (Cortex-M0+):

```sh
export CPU_TARGET=rp2040
./build.sh pow_int_cf.c
```

or

```sh
export CPU_TARGET=rp2040
./build_mand.sh
```

## Rolling your own CSUB

1. Write a C/assembler routine signature like `long long myfunc(void *arg0, ...);` and include
   `../PicoMiteAllVersions/PicoCFunctions.h` for helper macros.
2. Add the source (`foo_cf.c`) here, then run `./build.sh foo_cf.c`.
3. Format the binary into a CSUB block: `./emit_cfunction_block.sh foo_cf.bin "foo integer, ..."`.
4. Paste it between `CSUB …` / `End CSUB` in your BASIC program (or write a helper script like above).
5. Call it command-style, passing all parameters (pointers/values) in the order you declared.

With that pattern you can swap out any slow loop—fixed-point math, pixel plots, FFT, you name it—without
touching the firmware. The build helpers take care of the heavy lifting: Cross-compile, emit the hex block, and
patch your BASIC source so it’s always in sync with the latest native code.

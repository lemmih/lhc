[![Build Status](https://travis-ci.org/Lemmih/lhc.svg?branch=master)](https://travis-ci.org/Lemmih/lhc)

# About LHC

The LHC Haskell Compiler aims at compiling Haskell2010 to LLVM IR using the
[HaskellSuite](https://github.com/haskell-suite) of libraries.

## Getting started

LHC is built by stack:

```
stack setup
stack build
stack exec lhc
```

Installing the lhc-prim library is required before the example programs can be
compiled.

```
stack install cabal-install
stack exec -- cabal update
cd packages/lhc-prim/
stack exec -- cabal install --haskell-suite -w `stack exec -- which lhc`
```

After lhc-prim has been installed, all the programs in `examples/` will be
compilable:

```
stack exec -- lhc build examples/HelloWorld.hs
lli examples/HelloWorld.ll
```

You can run `lhc` with the `--keep-intermediate-files` flag to inspect the
various stages of the transformation from bedrock to llvm.

```
stack exec -- lhc build --verbose --keep-intermediate-files examples/HelloWorld.hs
```

## Testing

Running the test suite is done via stack:

```
stack test && cat .stack-work/logs/lhc-*-test.log
```

## Status (updated 2018-05-26)

LHC is comprised of separate libraries which are developed independenly and are
in various states of completion.

### haskell-scope

Haskell-scope implements name resolution for a sizable subset of Haskell2010 but
it's far from complete. So far it hasn't proved a bottleneck.

### haskell-tc

Haskell-tc implements type-checking and type-inference for a small subset of
Haskell2010. Only the constructs used in the example programs are supported.

### haskell-crux

Haskell-crux is responsible for desugaring and, like haskell-tc, has only been
defined for exactly those language constructs which are currently used by the
example programs.

### base library

The base is extremely minimal and functions are implemented on a by-need basis.

### Integer support

None. Will eventually write a minimal implementation in either Haskell or C.

### Exceptions

Not implemented yet but the RTS has been designed with exceptions in mind.

### Garbage collection

LHC has a simple but accurate semispace garbage collector.

### Threading

Not supported. Will eventually use libuv.

### Unicode support

Not supported. Will eventually use libicu.

## Compilation pipeline and the relevant libraries:

```
┌──────────────────┐
│     Parsing      │
│ haskell-src-exts │
└────────┬─────────┘
┌────────┴────────┐
│ Name resolution │
│  haskell-scope  │
└────────┬────────┘
 ┌───────┴───────┐
 │ Type-checking │
 │  haskell-tc   │
 └───────┬───────┘
  ┌──────┴───────┐
  │  Desugaring  │
  │ haskell-crux │
  └──────┬───────┘
┌────────┴────────┐
│ Optimizing Core │
│       LHC       │
└────────┬────────┘
    ┌────┴────┐
    │ Bedrock │
    │ bedrock │
    └────┬────┘
 ┌───────┴───────┐
 │ Generate LLVM │
 │    bedrock    │
 └───────────────┘
```

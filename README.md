[![Build Status](https://travis-ci.org/Lemmih/lhc.svg?branch=master)](https://travis-ci.org/Lemmih/lhc)

# About LHC

The LHC Haskell Compiler aims at compiling Haskell2010 to LLVM IR using
the [HaskellSuite](https://github.com/haskell-suite) of libraries.

## Getting started

LHC is built by stack:

```
stack setup
stack build
stack exec lhc
```

Installing the lhc-prim library is required before the example programs can be compiled.

```
stack install cabal-install
stack exec -- cabal update
cd packages/lhc-prim/
stack exec -- cabal install --haskell-suite -w `stack exec -- which lhc`
```

After lhc-prim has been installed, all the programs in `examples/` will be compilable:

```
stack exec -- lhc build examples/HelloWorld.hs
lli examples/HelloWorld.ll
```

You can run `lhc` with the `--keep-intermediate-files` flag to inspect the various
stages of the transformation from bedrock to llvm.

```
stack exec -- lhc build --verbose --keep-intermediate-files examples/HelloWorld.hs
```

## Testing

Running the test suite is done via stack:

```
stack test && cat .stack-work/logs/lhc-*-test.log
```

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
  │ haskell-core │
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

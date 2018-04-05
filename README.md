# LHC

LHC is a Haskell2010 compiler composed from smaller, reusable libraries.

## Setup

```
stack setup
stack build
stack exec lhc
```

## Installing lhc-prim

```
stack install cabal-install
stack exec -- cabal update
cd packages/lhc-prim/
stack exec -- cabal install --haskell-suite -w `stack exec -- which lhc`
```

## Compiling and running Hello.hs

```
stack exec -- lhc build Hello.hs
lli Hello.ll
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

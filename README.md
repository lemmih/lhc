# LHC

LHC is a Haskell2010 compiler composed from smaller, reusable libraries.

## Setup

```
stack setup
stack build
stack exec lhc
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

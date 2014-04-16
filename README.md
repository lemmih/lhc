# The Luxurious Haskell Compiler.

LHC is an optimizing Haskell compiler.

## Features.

TBD.

## Wish list.

The wish list is ordered by priority.

- Single-threaded event driven IO manager.
- Green threads.
- Exceptions.
- Arbitrary precision integers.
- Generational garbage collection.
- Shared-nothing multitasking.
- Haskell2010.
- GADTs.
- TemplateHaskell.
- MultiParamTypeClasses.
- TypeFamilies.
- OverloadedStrings.
- Superoptimization.
- User-friendly type-checking.
- Whole-program optimization.
- Immix GC.

## Implementation overview.

### Cabal integration and package management.

LHC uses haskell-packages (part of the Haskell Suite) to seemlessly integrate with Cabal.

### Exceptions.

For reasons of simplicity, exception handling is not implemented through stack inspection. Rather, exceptional values are passed through the call stack like regular values. The overhead of checking for exceptional values after each function call should be minimal since returned values are not placed on the heap.

### IO Manager.

IO primitives are event-driven and implemented via libuv.

### Shared-nothing asynchronous mesage passing.

TBD.

### Garbage collection.

TBD.

### Arbitrary precision integers.

There are three options here:
 1. A Haskell implementation.
 2. libGMP.
 3. libTommath.

As of yet, the available Haskell implementations are several orders of magnitude slower than the C implementations.

GMP is unwieldy and licensed under LGPL. Additionally, it does not play well with garbage collectors.

Tommath is a small library written in C with no licensing restrictions. It's decently quick and it integrates fairly well with garbage collectors. This is the library used by LHC for arbitrary precision integers.

## Implementation details.

### Throwing exceptions to threads.

TBD.

### IO Monad.

TBD.

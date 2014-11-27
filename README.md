# The LLVM Haskell Compiler.

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
- Supercompilation.
- User-friendly type-checking.
- Whole-program optimization.
- Immix GC.

## Implementation overview.

### Cabal integration and package management.

LHC uses haskell-packages (part of the Haskell Suite) to seemlessly integrate with Cabal.

### Exceptions.

Exceptions are handled by walking the list of stack frames until a handler
is found.

### IO Manager.

IO primitives are event-driven and implemented via libuv.

### Shared-nothing asynchronous mesage passing.

TBD.

### Garbage collection.

Garbage collection is handled in ordinary LLVM core. If an allocation fails,
we then mark the nodes (ie. objects that live in registers or on the stack)
and heap pointers. After the GC has completed, we call the current continuation
with the updated nodes and heap pointers.

GC strategies are built as plugins.

#### Fixed GC

This GC strategy allocates a block of memory at program start and never reclaims it.

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

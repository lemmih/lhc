# The Luxurious Haskell Compiler.

LHC is an optimizing Haskell compiler.

## Features.

TBD.

## Wish list.

The wish list is ordered by priority.

- Single-threaded event driven IO manager.
- Green threads.
- Exceptions.
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

IO and concurrency primitives (forkIO, MVar, etc) are implemented via libuv. 

### Shared-nothing asynchronous mesage passing.

TBD.

### Garbage collection.

TBD.

## Implementation details.

### Throwing exceptions to threads.

TBD.

### IO Monad.

TBD.

# Step by step guide to compiling Haskell

How to compile Haskell to LLVM in 14 simple steps.

## Step 1: Parsing

Parsing our Haskell code with `haskell-src-exts` is the first step in our
pipeline. We'll use `Haskell.Language.Exts.parseFile` to turn this source file
into an AST:

```haskell
{-# LANGUAGE MagicHash #-}
module Main (main) where

import LHC.Prim

main :: IO ()
main = do
  putStrLn "What is your name?"
  name <- getLine
  putStr "Hi "
  putStr name
  putStrLn "."

entrypoint :: ()
entrypoint = unsafePerformIO main
```

The resulting type is `Module SrcSpanInfo`, ie. a module annotated with source
locations.

## Step 2: Scoping

Next step is applying scoping rules to figure out which identifiers refer to
which bindings. For this, we'll use `haskell-scope` which turns a
`Module SrcSpanInfo` into a `Module Origin`.

`Origin` contains a `NameInfo` which tells us where each identifier is used:

```haskell
data Origin = Origin NameInfo SrcSpanInfo
data NameInfo
  = Resolved Entity
  | Binding Entity
  | None
  | ScopeError ScopeError
```

## Step 3: Type checking

Type-checking is performed by `haskell-tc` which turns a `Module Origin` into
a `Module Typed`. After a successful type check, each identifier has been given
a type and each usage site has been annotated with a proof that the types are
correct.

To understand the output of the type-checker, consider the following example:

```haskell
x = length "Hello world"
```

After type-checking, `length` has been annotated with a proof (`@Char`) which
turns its type `forall a. [a] -> Int` into `[Char] -> Int` to match the string
argument. In this case the proof is rather simple but they can become quite
complicated once higher-rank types come into play.

```haskell
x :: Int
x = (length :: forall a. [a] -> Int) @Char "Hello world"
```

## Step 4: Desugaring

Haskell has a lot of syntaxtic sugar that can be simplified away. If-then-else
expressions are replaced by case-expressions, pattern matches in function
definitions are replaced by explicit case-expressions, do-notation is replaced
by calls to `bind` and `then`, etc.

We end up with a lazy, functional language that looks like a plain version of
Haskell. At this point we also perform a few optimizations designed to improve
the readability of the generated code.

```haskell
[library code has been omitted]
Main.main : LHC.Prim.IO ()
Main.main =
  λ LHC.Prim.s:LHC.Prim.RealWorld# →
  LHC.Prim.thenIO (LHC.Prim.putStrLn (LHC.Prim.unpackString# "What is your name?")) (LHC.Prim.bindIO
          LHC.Prim.getLine (λ Main.name:[LHC.Prim.Char] LHC.Prim.s:LHC.Prim.RealWorld# →
        LHC.Prim.thenIO (LHC.Prim.putStr (LHC.Prim.unpackString# "Hi ")) (LHC.Prim.thenIO
                (LHC.Prim.putStr Main.name) (LHC.Prim.putStrLn (LHC.Prim.unpackString# ".")))
          LHC.Prim.s)) LHC.Prim.s
Main.entrypoint : ()
Main.entrypoint =
  LHC.Prim.unsafePerformIO
```

## Step 5: High level intermediate language

Core -> Bedrock

## Step 6: Remove `@eval` and `@apply` primitives

Lower EvalApply

## Step 7: Make node sizes explicit

Lower NodeSize

## Step 8: Put nodes in word-sized variables

Register introduction

## Step 9: Make the stack layout explicit

Make stack explicit

## Step 10: Turn all function calls into tail calls with CPS

CPS transform

## Step 11: Remove `@alloc` primitives

Lower allocs

## Step 12: Plugging in a garbage collector

Lower gc primitives

## Step 13: Remove global registers

Lower globals

## Step 14: Generating LLVM IR

Generate LLVM

## Missing pieces

### Finding pointers in activation frames

### Raising and catching exceptions

### Threading

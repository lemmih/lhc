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

Now it's time to take our high level code from the last step and gradually
remove all of the advaned features (laziness, higher-order functions, exceptions,
garbage collection, etc) until we can pretty-print it as LLVM IR.

We'll use a new language called Bedrock. This is a strict, first-order language
with a lot of built-in primitives to support graph reduction. It's a re-imagining
of Boquist's GRIN language.

When translating Core to Bedrock, we use the `@store`, `@eval` and `@apply`
primitives to allocate nodes on the heap and to implement laziness. Read more
about eval/apply here: https://www.microsoft.com/en-us/research/publication/make-fast-curry-pushenter-vs-evalapply/

```
foreign i32 putchar(i32)
foreign i8 indexI8#(i8*)
foreign i8* addrAdd#(i8*, i64)
foreign i32 getchar()
node LHC.Prim.Int32(i32)
node LHC.Prim.C#^1(i32)
node LHC.Prim.Unit^2()
node LHC.Prim.Nil^3()
node LHC.Prim.Cons^4(*, *)
entrypoint: Main.entrypoint^17

void * LHC.Prim.c_putchar^5 i32|arg^18 void|s^19 =
  i32|primOut^20 = @ccall putchar(i32|arg^18)
  *con = @store (LHC.Prim.Int32 i32|primOut^20)
  @return void|s^19, *con

void * LHC.Prim.c_getchar^6 void|s^22 =
  i32|primOut^23 = @ccall getchar()
  *con^1 = @store (LHC.Prim.Int32 i32|primOut^23)
  @return void|s^22, *con^1

void * LHC.Prim.getChar.lambda^7 *LHC.Prim.c^2 void|LHC.Prim.s^26 =
  *LHC.Prim.c.eval^3 = @eval *LHC.Prim.c^2
  %LHC.Prim.c.eval.node = @fetch   *LHC.Prim.c.eval^3
  LHC.Prim.Int32 i32|LHC.Prim.c#^29 ←  %LHC.Prim.c.eval.node
  *con^4 = @store (LHC.Prim.C#^1 i32|LHC.Prim.c#^29)
  @return void|LHC.Prim.s^26, *con^4

void * LHC.Prim.getChar^8 void|LHC.Prim.s^31 =
  void|ret.apply^32, *ret.apply^5 = LHC.Prim.c_getchar^6(void|LHC.Prim.s^31)
  @tail LHC.Prim.getChar.lambda^7(*ret.apply^5, void|ret.apply^32)

void * LHC.Prim.getLine.lambda^9 *LHC.Prim.c^6 *LHC.Prim.cs^7 void|LHC.Prim.s^36 =
  *con^8 = @store (LHC.Prim.Cons^4 *LHC.Prim.c^6 *LHC.Prim.cs^7)
  @return void|LHC.Prim.s^36, *con^8

void * LHC.Prim.getLine.lambda^10 *LHC.Prim.c^9 void|LHC.Prim.s^39 =
  *LHC.Prim.c.eval^10 = @eval *LHC.Prim.c^9
  %LHC.Prim.c.eval.node^1 = @fetch   *LHC.Prim.c.eval^10
  LHC.Prim.C#^1 i32|LHC.Prim.c#^42 ←  %LHC.Prim.c.eval.node^1
  case i32|LHC.Prim.c#^42 of
    10 →
      *con^12 = @store (LHC.Prim.Nil^3)
      @return void|LHC.Prim.s^39, *con^12
    DEFAULT →
      void|ret.apply^43, *ret.apply^11 = LHC.Prim.getLine^11(void|LHC.Prim.s^39)
      @tail LHC.Prim.getLine.lambda^9(*LHC.Prim.c^9, *ret.apply^11, void|ret.apply^43)

void * LHC.Prim.getLine^11 void|LHC.Prim.s^46 =
  void|ret.apply^47, *ret.apply^13 = LHC.Prim.getChar^8(void|LHC.Prim.s^46)
  @tail LHC.Prim.getLine.lambda^10(*ret.apply^13, void|ret.apply^47)

void * LHC.Prim.putStr^12 *LHC.Prim.lst^14 void|LHC.Prim.s^50 =
  *LHC.Prim.lst.eval^15 = @eval *LHC.Prim.lst^14
  %LHC.Prim.lst.eval.node^2 = @fetch   *LHC.Prim.lst.eval^15
  case %LHC.Prim.lst.eval.node^2 of
    LHC.Prim.Nil^3 →
      *con^16 = @store (LHC.Prim.Unit^2)
      @return void|LHC.Prim.s^50, *con^16
    LHC.Prim.Cons^4 *LHC.Prim.head^17 *LHC.Prim.tail^18 →
      *LHC.Prim.head.eval^19 = @eval *LHC.Prim.head^17
      %LHC.Prim.head.eval.node^3 = @fetch   *LHC.Prim.head.eval^19
      LHC.Prim.C#^1 i32|LHC.Prim.char^58 ←  %LHC.Prim.head.eval.node^3
      void|ret.apply^59, *ret.apply^20 = LHC.Prim.c_putchar^5(i32|LHC.Prim.char^58, void|LHC.Prim.s^50)
      @tail LHC.Prim.putStr^12(*LHC.Prim.tail^18, void|ret.apply^59)

* LHC.Prim.unpackString#^13 i8*|LHC.Prim.ptr^61 =
  i8|primOut^62 = @ccall indexI8#(i8*|LHC.Prim.ptr^61)
  i64|arg.val^63 = @cast(i8|primOut^62)
  case i64|arg.val^63 of
    0 →
      *con^24 = @store (LHC.Prim.Nil^3)
      @return *con^24
    DEFAULT →
      i32|arg.val^64 = @cast(i64|arg.val^63)
      *con^21 = @store (LHC.Prim.C#^1 i32|arg.val^64)
      i64|int^66 = @literal 1
      i8*|primOut^67 = @ccall addrAdd#(i8*|LHC.Prim.ptr^61, i64|int^66)
      *thunk^22 = @store (LHC.Prim.unpackString#^13 i8*|primOut^67)
      *con^23 = @store (LHC.Prim.Cons^4 *con^21 *thunk^22)
      @return *con^23

void * LHC.Prim.putStrLn^14 *LHC.Prim.msg^25 void|LHC.Prim.s^72 =
  i8*|lit^73 = @literal "\n"
  *thunk^26 = @store (LHC.Prim.unpackString#^13 i8*|lit^73)
  void|ret.apply^75, *ret.apply^27 = LHC.Prim.putStr^12(*LHC.Prim.msg^25, void|LHC.Prim.s^72)
  @tail LHC.Prim.putStr^12(*thunk^26, void|ret.apply^75)

void * Main.main.lambda^15 *Main.name^28 void|LHC.Prim.s^78 =
  i8*|lit^79 = @literal "Hi "
  *thunk^29 = @store (LHC.Prim.unpackString#^13 i8*|lit^79)
  i8*|lit^81 = @literal "."
  *thunk^30 = @store (LHC.Prim.unpackString#^13 i8*|lit^81)
  void|ret.apply^83, *ret.apply^31 = LHC.Prim.putStr^12(*thunk^29, void|LHC.Prim.s^78)
  void|ret.apply^85, *ret.apply^32 = LHC.Prim.putStr^12(*Main.name^28, void|ret.apply^83)
  @tail LHC.Prim.putStrLn^14(*thunk^30, void|ret.apply^85)

void * Main.main^16 void|LHC.Prim.s^87 =
  i8*|lit^88 = @literal "What is your name?"
  *thunk^33 = @store (LHC.Prim.unpackString#^13 i8*|lit^88)
  void|ret.apply^90, *ret.apply^34 = LHC.Prim.putStrLn^14(*thunk^33, void|LHC.Prim.s^87)
  void|ret.apply^92, *ret.apply^35 = LHC.Prim.getLine^11(void|ret.apply^90)
  @tail Main.main.lambda^15(*ret.apply^35, void|ret.apply^92)

* Main.entrypoint^17  =
  void|void^94 = @literal 0
  void|ret.apply^95, *ret.apply^36 = Main.main^16(void|void^94)
  *LHC.Prim.val.eval^37 = @eval *ret.apply^36
```

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

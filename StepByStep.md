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

The entire bedrock code (including all library code) for our original Haskell
program looks like this:

```
foreign i32 putchar(i32)
foreign i8 indexI8#(i8*)
foreign i8* addrAdd#(i8*, i64)
foreign i32 getchar()
node LHC.Prim.Int32(i32)
node LHC.Prim.C#(i32)
node LHC.Prim.Unit()
node LHC.Prim.Nil()
node LHC.Prim.Cons(*, *)
entrypoint: Main.entrypoint

void * LHC.Prim.c_putchar i32|arg void|s =
  i32|primOut = @ccall putchar(i32|arg)
  *con = @store (LHC.Prim.Int32 i32|primOut)
  @return void|s, *con

void * LHC.Prim.c_getchar void|s =
  i32|primOut = @ccall getchar()
  *con = @store (LHC.Prim.Int32 i32|primOut)
  @return void|s, *con

void * LHC.Prim.getChar.lambda *LHC.Prim.c void|LHC.Prim.s =
  *LHC.Prim.c.eval = @eval *LHC.Prim.c
  %LHC.Prim.c.eval.node = @fetch   *LHC.Prim.c.eval
  LHC.Prim.Int32 i32|LHC.Prim.c# ←  %LHC.Prim.c.eval.node
  *con = @store (LHC.Prim.C# i32|LHC.Prim.c#)
  @return void|LHC.Prim.s, *con

void * LHC.Prim.getChar void|LHC.Prim.s =
  void|ret.apply, *ret.apply^1 = LHC.Prim.c_getchar(void|LHC.Prim.s)
  @tail LHC.Prim.getChar.lambda(*ret.apply^1, void|ret.apply)

void * LHC.Prim.getLine.lambda *LHC.Prim.c *LHC.Prim.cs void|LHC.Prim.s =
  *con = @store (LHC.Prim.Cons *LHC.Prim.c *LHC.Prim.cs)
  @return void|LHC.Prim.s, *con

void * LHC.Prim.getLine.lambda^1 *LHC.Prim.c void|LHC.Prim.s =
  *LHC.Prim.c.eval = @eval *LHC.Prim.c
  %LHC.Prim.c.eval.node = @fetch   *LHC.Prim.c.eval
  LHC.Prim.C# i32|LHC.Prim.c# ←  %LHC.Prim.c.eval.node
  case i32|LHC.Prim.c# of
    10 →
      *con = @store (LHC.Prim.Nil)
      @return void|LHC.Prim.s, *con
    DEFAULT →
      void|ret.apply, *ret.apply^1 = LHC.Prim.getLine(void|LHC.Prim.s)
      @tail LHC.Prim.getLine.lambda(*LHC.Prim.c, *ret.apply^1, void|ret.apply)

void * LHC.Prim.getLine void|LHC.Prim.s =
  void|ret.apply, *ret.apply^1 = LHC.Prim.getChar(void|LHC.Prim.s)
  @tail LHC.Prim.getLine.lambda^1(*ret.apply^1, void|ret.apply)

void * LHC.Prim.putStr *LHC.Prim.lst void|LHC.Prim.s =
  *LHC.Prim.lst.eval = @eval *LHC.Prim.lst
  %LHC.Prim.lst.eval.node = @fetch   *LHC.Prim.lst.eval
  case %LHC.Prim.lst.eval.node of
    LHC.Prim.Nil →
      *con = @store (LHC.Prim.Unit)
      @return void|LHC.Prim.s, *con
    LHC.Prim.Cons *LHC.Prim.head *LHC.Prim.tail →
      *LHC.Prim.head.eval = @eval *LHC.Prim.head
      %LHC.Prim.head.eval.node = @fetch   *LHC.Prim.head.eval
      LHC.Prim.C# i32|LHC.Prim.char ←  %LHC.Prim.head.eval.node
      void|ret.apply, *ret.apply^1 = LHC.Prim.c_putchar(i32|LHC.Prim.char, void|LHC.Prim.s)
      @tail LHC.Prim.putStr(*LHC.Prim.tail, void|ret.apply)

* LHC.Prim.unpackString# i8*|LHC.Prim.ptr =
  i8|primOut = @ccall indexI8#(i8*|LHC.Prim.ptr)
  i64|arg.val = @cast(i8|primOut)
  case i64|arg.val of
    0 →
      *con^2 = @store (LHC.Prim.Nil)
      @return *con^2
    DEFAULT →
      i32|arg.val^1 = @cast(i64|arg.val)
      *con = @store (LHC.Prim.C# i32|arg.val^1)
      i64|int = @literal 1
      i8*|primOut^1 = @ccall addrAdd#(i8*|LHC.Prim.ptr, i64|int)
      *thunk = @store (LHC.Prim.unpackString# i8*|primOut^1)
      *con^1 = @store (LHC.Prim.Cons *con *thunk)
      @return *con^1

void * LHC.Prim.putStrLn *LHC.Prim.msg void|LHC.Prim.s =
  i8*|lit = @literal "\n"
  *thunk = @store (LHC.Prim.unpackString# i8*|lit)
  void|ret.apply, *ret.apply^1 = LHC.Prim.putStr(*LHC.Prim.msg, void|LHC.Prim.s)
  @tail LHC.Prim.putStr(*thunk, void|ret.apply)

void * Main.main.lambda *Main.name void|LHC.Prim.s =
  i8*|lit = @literal "Hi "
  *thunk = @store (LHC.Prim.unpackString# i8*|lit)
  i8*|lit^1 = @literal "."
  *thunk^1 = @store (LHC.Prim.unpackString# i8*|lit^1)
  void|ret.apply, *ret.apply^1 = LHC.Prim.putStr(*thunk, void|LHC.Prim.s)
  void|ret.apply^2, *ret.apply^3 = LHC.Prim.putStr(*Main.name, void|ret.apply)
  @tail LHC.Prim.putStrLn(*thunk^1, void|ret.apply^2)

void * Main.main void|LHC.Prim.s =
  i8*|lit = @literal "What is your name?"
  *thunk = @store (LHC.Prim.unpackString# i8*|lit)
  void|ret.apply, *ret.apply^1 = LHC.Prim.putStrLn(*thunk, void|LHC.Prim.s)
  void|ret.apply^2, *ret.apply^3 = LHC.Prim.getLine(void|ret.apply)
  @tail Main.main.lambda(*ret.apply^3, void|ret.apply^2)

* Main.entrypoint  =
  void|void = @literal 0
  void|ret.apply, *ret.apply^1 = Main.main(void|void)
  *LHC.Prim.val.eval = @eval *ret.apply^1
```

## Step 6: Remove `@eval` and `@apply` primitives

GHC implements the `@eval` and `@apply` primitives in its runtime-system but
LHC has a different strategy in mind. By analysis the code, we can replace
each call to `@eval` and `@apply` with new function. One way of doing this is
with a heap points-to (HPT) analysis but this is rather expensive. At the moment,
LHC is using a cheaper algorithm at the cost of generated less optimized code.

After the transformation, all calls to `@eval` have been replaced by a real
`eval` function. The `@apply` primitive is not used by our example program:

```
[code omitted]
* eval *arg =
  %obj = @fetch   *arg
  case %obj of
    LHC.Prim.unpackString# i8*|arg^1 →
      @tail LHC.Prim.unpackString#(i8*|arg^1)
    Main.entrypoint →
      @tail Main.entrypoint()
    DEFAULT →
      @return *arg

[code omitted]

* LHC.Prim.putStr *LHC.Prim.lst =
  *LHC.Prim.lst.eval = eval(*LHC.Prim.lst)
  %LHC.Prim.lst.eval.node = @fetch   *LHC.Prim.lst.eval
  case %LHC.Prim.lst.eval.node of
    LHC.Prim.Nil →
      *con = @store (LHC.Prim.Unit)
      @return *con
    LHC.Prim.Cons *LHC.Prim.head *LHC.Prim.tail →
      *LHC.Prim.head.eval = eval(*LHC.Prim.head)
      %LHC.Prim.head.eval.node = @fetch   *LHC.Prim.head.eval
      LHC.Prim.C# i32|LHC.Prim.char ←  %LHC.Prim.head.eval.node
      *ret.apply = LHC.Prim.c_putchar(i32|LHC.Prim.char)
      @tail LHC.Prim.putStr(*LHC.Prim.tail)

[code omitted]

* Main.entrypoint  =
  *ret.apply = Main.main()
  *LHC.Prim.val.eval = eval(*ret.apply)
  @exit
```

## Step 7: Make node sizes explicit

Next we try to determine the sizes of nodes. Like with the previous step, there
are many algorithms for this and they have different cost/benefit ratios.
LHC is using a rather conservative algorithm which is less accurate than the
algorithm described by Boquist.

The following code shows how the `obj` variable has been marked with a size of
`2` and `lst.eval.node` has size `3`:

```
* eval *arg =
  %2%obj = @fetch   *arg
  case %2%obj of
    LHC.Prim.unpackString# i8*|arg^1 →
      @tail LHC.Prim.unpackString#(i8*|arg^1)
    Main.entrypoint →
      @tail Main.entrypoint()
    DEFAULT →
      @return *arg

* LHC.Prim.putStr *LHC.Prim.lst =
  *LHC.Prim.lst.eval = eval(*LHC.Prim.lst)
  %3%LHC.Prim.lst.eval.node = @fetch   *LHC.Prim.lst.eval
  case %3%LHC.Prim.lst.eval.node of
    LHC.Prim.Nil →
      *con = @store (LHC.Prim.Unit)
      @return *con
    LHC.Prim.Cons *LHC.Prim.head *LHC.Prim.tail →
      *LHC.Prim.head.eval = eval(*LHC.Prim.head)
      %2%LHC.Prim.head.eval.node = @fetch   *LHC.Prim.head.eval
      LHC.Prim.C# i32|LHC.Prim.char ←  %2%LHC.Prim.head.eval.node
      *ret.apply = LHC.Prim.c_putchar(i32|LHC.Prim.char)
      @tail LHC.Prim.putStr(*LHC.Prim.tail)
```

## Step 8: Put nodes in word-sized variables

LLVM has no support for Haskell nodes so we have to put the data structure into
separate variables with one variable for each field:

```
* eval *arg =
  #obj = @load   *arg[0]
  #obj^1 = @load   *arg[1]
  case #obj of
    LHC.Prim.unpackString# →
      i8*|arg^1 = @cast(#obj^1)
      @tail LHC.Prim.unpackString#(i8*|arg^1)
    Main.entrypoint →
      @tail Main.entrypoint()
    DEFAULT →
      @return *arg

* LHC.Prim.putStr *LHC.Prim.lst =
  *LHC.Prim.lst.eval = eval(*LHC.Prim.lst)
  #LHC.Prim.lst.eval.node = @load   *LHC.Prim.lst.eval[0]
  #LHC.Prim.lst.eval.node^1 = @load   *LHC.Prim.lst.eval[1]
  #LHC.Prim.lst.eval.node^2 = @load   *LHC.Prim.lst.eval[2]
  case #LHC.Prim.lst.eval.node of
    LHC.Prim.Nil →
      @alloc 1
      *con = @store (LHC.Prim.Unit)
      @return *con
    LHC.Prim.Cons →
      *LHC.Prim.head = @cast(#LHC.Prim.lst.eval.node^1)
      *LHC.Prim.tail = @cast(#LHC.Prim.lst.eval.node^2)
      *LHC.Prim.head.eval = eval(*LHC.Prim.head)
      #LHC.Prim.head.eval.node = @load   *LHC.Prim.head.eval[0]
      #LHC.Prim.head.eval.node^1 = @load   *LHC.Prim.head.eval[1]
      LHC.Prim.C# ←  #LHC.Prim.head.eval.node
      i32|LHC.Prim.char = @cast(#LHC.Prim.head.eval.node^1)
      *ret.apply = LHC.Prim.c_putchar(i32|LHC.Prim.char)
      @tail LHC.Prim.putStr(*LHC.Prim.tail)
```

## Step 9: Make the stack layout explicit

The next couple of steps relate to stack management. While it is possible to
let LLVM handle the stack, this makes exceptions, garbage collection and threading
much more difficult. Therefore, we want to allocate our own activation frames
and only use the system stack for non-Haskell code.

After the first transformation, the code is annotated with spill primitives
(`@restore` and `@save`) which read and write variables to the current
activation frame.

```
* LHC.Prim.putStr *LHC.Prim.lst =
  *LHC.Prim.lst.eval = eval(*LHC.Prim.lst)
  #LHC.Prim.lst.eval.node = @load   *LHC.Prim.lst.eval[0]
  #LHC.Prim.lst.eval.node^1 = @load   *LHC.Prim.lst.eval[1]
  #LHC.Prim.lst.eval.node^2 = @load   *LHC.Prim.lst.eval[2]
  case #LHC.Prim.lst.eval.node of
    LHC.Prim.Nil →
      @alloc 1
      *con = @store (LHC.Prim.Unit)
      @return *con
    LHC.Prim.Cons →
      *LHC.Prim.head = @cast(#LHC.Prim.lst.eval.node^1)
      *LHC.Prim.tail = @cast(#LHC.Prim.lst.eval.node^2)
      @save[3] *LHC.Prim.tail
      *LHC.Prim.head.eval = eval(*LHC.Prim.head)
      *LHC.Prim.tail = @restore[3]
      #LHC.Prim.head.eval.node = @load   *LHC.Prim.head.eval[0]
      #LHC.Prim.head.eval.node^1 = @load   *LHC.Prim.head.eval[1]
      LHC.Prim.C# ←  #LHC.Prim.head.eval.node
      i32|LHC.Prim.char = @cast(#LHC.Prim.head.eval.node^1)
      *ret.apply = LHC.Prim.c_putchar(i32|LHC.Prim.char)
      *LHC.Prim.tail = @restore[3]
      @tail LHC.Prim.putStr(*LHC.Prim.tail)
```

## Step 10: Turn all function calls into tail calls with CPS

Once all the spills are explicit, we convert the code to continuation passing
style. This gets rid of plain function applications and replaces them with
continuations.

The `putStr` function from the previous step contains three function calls (`eval`
on line 2, `eval` on line 15, and `c_putchar` on line 21) so three continuations
will be created:

```
void LHC.Prim.putStr *LHC.Prim.lst @cont =
  @alloc 4
  @bedrock.stackframe = @store (bedrock.StackFrame)
  @bump 3
  @write @bedrock.stackframe[2] @cont
  void(i8*)*|fnPtr = &LHC.Prim.putStr.continuation
  @write @bedrock.stackframe[1] void(i8*)*|fnPtr
  @tail eval(*LHC.Prim.lst, @bedrock.stackframe)

void LHC.Prim.putStr.continuation @bedrock.stackframe.cont *LHC.Prim.lst.eval =
  @cont = @load   @bedrock.stackframe.cont[2]
  #LHC.Prim.lst.eval.node = @load   *LHC.Prim.lst.eval[0]
  #LHC.Prim.lst.eval.node^1 = @load   *LHC.Prim.lst.eval[1]
  #LHC.Prim.lst.eval.node^2 = @load   *LHC.Prim.lst.eval[2]
  case #LHC.Prim.lst.eval.node of
    LHC.Prim.Nil →
      @alloc 1
      *con = @store (LHC.Prim.Unit)
      #contNode = @load   @cont[1]
      @invoke #contNode(@cont, *con)
    LHC.Prim.Cons →
      *LHC.Prim.head = @cast(#LHC.Prim.lst.eval.node^1)
      *LHC.Prim.tail = @cast(#LHC.Prim.lst.eval.node^2)
      @write @bedrock.stackframe.cont[3] *LHC.Prim.tail
      void(i8*)*|fnPtr = &LHC.Prim.putStr.continuation^1
      @write @bedrock.stackframe.cont[1] void(i8*)*|fnPtr
      @tail eval(*LHC.Prim.head, @bedrock.stackframe.cont)

void LHC.Prim.putStr.continuation^1 @bedrock.stackframe.cont *LHC.Prim.head.eval =
  @cont = @load   @bedrock.stackframe.cont[2]
  *LHC.Prim.tail = @load   @bedrock.stackframe.cont[3]
  #LHC.Prim.head.eval.node = @load   *LHC.Prim.head.eval[0]
  #LHC.Prim.head.eval.node^1 = @load   *LHC.Prim.head.eval[1]
  LHC.Prim.C# ←  #LHC.Prim.head.eval.node
  i32|LHC.Prim.char = @cast(#LHC.Prim.head.eval.node^1)
  void(i8*)*|fnPtr = &LHC.Prim.putStr.continuation^2
  @write @bedrock.stackframe.cont[1] void(i8*)*|fnPtr
  @tail LHC.Prim.c_putchar(i32|LHC.Prim.char, @bedrock.stackframe.cont)

void LHC.Prim.putStr.continuation^2 @bedrock.stackframe.cont *ret.apply =
  @cont = @load   @bedrock.stackframe.cont[2]
  *LHC.Prim.tail = @load   @bedrock.stackframe.cont[3]
  @tail LHC.Prim.putStr(*LHC.Prim.tail, @cont)
```

## Step 11: Remove `@alloc` primitives

Now that the activations frames are explicitly allocated, we can get rid of the
`@alloc` primitive. We want to replace it with the `@gc_allocate` primitive but
this primitive can fail if the heap is full. So, we have to check for failure
and possibly initiate a garbage collection.

The general principle is illustrated by this example:

```
void function *arg =
  @alloc 4
  function_body_here

 ===>

void function *arg =
  #check <- @gc_allocate 4
  case #check of
    0 →
      @tail function.without_mem(*arg)
    1 →
      @tail function.with_mem(*arg)

; Ran out of heap, initiate a garbage collect
void function.without_mem *arg =
  @gc_begin
  *arg.marked = @gc_mark *arg
  @gc_end
  @tail function.with_mem(*arg.marked)

void function.with_mem *arg =
  function_body_here
```

## Step 12: Plugging in a garbage collector

Garbage collectors in LHC are written as plugins which generate bedrock code.
As of writing, there's only a single plugins which merely allocates a large slab
of memory and never does any collection.

## Step 13: Remove global registers

The garbage collect may use global register to store the heap pointer, heap limits
and other frequently access information. We make these registers explicit by
passing them as the first parameters to all the bedrock functions.

The following two snippets show how the virtual `hp` register is turned into
a variable which is explicitly passed around:

```
void LHC.Prim.c_putchar.with_mem i32|arg @cont =
  i32|primOut = @ccall putchar(i32|arg)
  *hp = @register hp
  *con = @cast(*hp)
  %tmp = @node (LHC.Prim.Int32)
  @write *hp[0] %tmp
  @write *hp[1] i32|primOut
  *hp' = &*hp[2]
  @register hp = *hp'
  #contNode = @load   @cont[1]
  @invoke #contNode(@cont, *con)
```

```
void LHC.Prim.c_putchar.with_mem *hp i32|arg @cont =
  i32|primOut = @ccall putchar(i32|arg)
  %tmp = @node (LHC.Prim.Int32)
  @write *hp[0] %tmp
  @write *hp[1] i32|primOut
  *hp' = &*hp[2]
  #contNode = @load   @cont[1]
  @invoke #contNode(*hp', @cont, *hp)
```

## Step 14: Generating LLVM IR

The bedrock code is very low level at this point and can be directly pretty-printed
as LLVM IR.

Bedrock function:

```
void eval *hp *arg @cont =
  #obj = @load   *arg[0]
  #obj^1 = @load   *arg[1]
  case #obj of
    LHC.Prim.unpackString# _ →
      i8*|arg^1 = @cast(#obj^1)
      @tail LHC.Prim.unpackString#(*hp, i8*|arg^1, @cont)
    Main.entrypoint _ →
      @tail Main.entrypoint(*hp, @cont)
    DEFAULT →
      #contNode = @load   @cont[1]
      @invoke #contNode(*hp, @cont, *arg)
```

LLVM function:

```llvm
define internal fastcc void @eval(i64* %hp, i64* %arg, i64* %cont) nounwind{
; <label>:0:
  %1 = getelementptr inbounds i64, i64* %arg, i64 0
  %2 = load i64, i64* %1, align 1
  %obj = bitcast i64 %2 to i64
  %3 = getelementptr inbounds i64, i64* %arg, i64 1
  %4 = load i64, i64* %3, align 1
  %obj_1 = bitcast i64 %4 to i64
  switch i64 %obj, label %5 [i64 108, label %"LHC.Prim.unpackString# _" i64 185, label %"Main.entrypoint _"]
"LHC.Prim.unpackString# _":
  %arg_1 = inttoptr i64 %obj_1 to i8*
  tail call fastcc void @"LHC.Prim.unpackString#"(i64* %hp, i8* %arg_1, i64* %cont) nounwind
  ret void
"Main.entrypoint _":
  tail call fastcc void @Main.entrypoint(i64* %hp, i64* %cont) nounwind
  ret void
; <label>:5:
  %6 = getelementptr inbounds i64, i64* %cont, i64 1
  %7 = load i64, i64* %6, align 1
  %contNode = bitcast i64 %7 to i64
  %8 = inttoptr i64 %contNode to void (i64*, i64*, i64*)*
  tail call fastcc void %8(i64* %hp, i64* %cont, i64* %arg) nounwind
  unreachable
}
```

## Missing pieces

### Finding pointers in activation frames

### Raising and catching exceptions

### Threading

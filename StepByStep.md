# Step by step guide to compiling Haskell

## Step 1: Parsing
String -> AST

## Step 2: Scoping
AST -> Scoped

## Step 3: Type checking
Scoped -> Typed

## Step 4: Desugaring
Typed -> Core

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

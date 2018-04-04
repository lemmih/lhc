{-|
Void elimination removes all variables with the void type. This pass has to be
run /after/ 'eval' and 'apply' has been lowered since those functions have to
pretend void types actually exist.

The imaginary 'RealWorld#' parameter of the IO monad is implemented as a void
type.
-}
module Data.Bedrock.VoidElimination ( voidEliminate ) where

import Data.Bedrock

voidEliminate :: Module -> Module
voidEliminate m = m
  { functions = map voidFunction (functions m) }

voidFunction :: Function -> Function
voidFunction fn = fn
  { fnArguments = voidVariables (fnArguments fn)
  , fnResults   = filter (not.isVoidType) (fnResults fn)
  , fnBody      = voidBlock (fnBody fn) }

voidBlock :: Block -> Block
voidBlock block =
  case block of
    Case scrut mbDefaultBranch alts ->
      Case
        scrut
        (fmap voidBlock mbDefaultBranch)
        [ Alternative (voidPattern pattern) (voidBlock branch)
        | Alternative pattern branch <- alts ]
    Bind binds expr rest ->
      Bind
        (voidVariables binds)
        (voidExpression expr)
        (voidBlock rest)
    Return vars -> Return (voidVariables vars)
    Raise{} -> block
    TailCall fn vars -> TailCall fn (voidVariables vars)
    Invoke fn vars -> Invoke fn (voidVariables vars)
    InvokeHandler{} -> block
    Exit    -> block
    Panic{} -> block

voidPattern :: Pattern -> Pattern
voidPattern pattern =
  case pattern of
    NodePat name vars ->
      NodePat name (voidVariables vars)
    LitPat{} -> pattern


voidExpression :: Expression -> Expression
voidExpression expr =
  case expr of
    Application fn vars ->
      Application fn (voidVariables vars)
    CCall fn vars ->
      CCall fn (voidVariables vars)
    --Catch Name [Variable] Name [Variable]
    --Alloc Int
    Store name vars ->
      Store name (voidVariables vars)
    --BumpHeapPtr Int
    --Write Variable Int Variable
    --Address Variable Int
    --FunctionPointer Name
    --Fetch MemAttributes Variable
    --Load MemAttributes Variable Int
    --Add Variable Variable
    --Undefined
    --Save Variable Int
    --Restore Int
    --ReadRegister String
    --WriteRegister String Variable
    --ReadGlobal String
    --WriteGlobal String Variable
    --TypeCast Variable
    MkNode name vars ->
      MkNode name (voidVariables vars)
    --Literal Literal
    --Eval Variable
    --Apply Variable Variable
    --GCAllocate Int
    --GCBegin
    --GCEnd
    --GCMark Variable
    --GCMarkNode Variable
    _ -> expr

voidVariables :: [Variable] -> [Variable]
voidVariables = filter (not.isVoidVariable)

isVoidType :: Type -> Bool
isVoidType (Primitive CVoid) = True
isVoidType _ = False

isVoidVariable :: Variable -> Bool
isVoidVariable = isVoidType . variableType

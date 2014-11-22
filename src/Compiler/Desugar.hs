module Compiler.Desugar where

import Language.Haskell.Exts.Annotated
import Language.Haskell.Names

import Control.Monad.State

--import qualified Language.Haskell.Exts.Annotated as HSE

--data Desugar = Desugar [Decl]

--data Ident = Ident
--data Decl = Node Ident [Type] | Function Ident [Ident] Expression

--data Type = PtrType | WordType | NodeType

--data Lit =
--  Lint Integer |
--  Lrational Rational |
--  Lchar Char |
--  Lstring String
--  deriving ( Show )

--data Value
--  = Empty
--  | Lit Lit
--  | Constructor Ident [Ident]


--data Alt = Value :> Expression

--data Expression =
--  Unit Value |
--  Bind Expression Expression |
--  Let [(Ident, Ident, [Ident])] Expression |
--  Application Ident [Ident] |
--  Case Ident [Alt] (Maybe Expression)


type Ann = Scoped SrcSpan
type Desugar a = State Int a

noScope :: Scoped SrcSpan
noScope = Scoped None (mkSrcSpan noLoc noLoc)

newName :: Desugar (Name Ann)
newName = do
  u <- get
  put $! u+1
  return (Ident noScope $ "desugar_" ++ show u)

newNameFrom :: Name Ann -> Desugar (Name Ann)
newNameFrom prefix = do
  u <- get
  put $! u+1
  case prefix of
    Ident scope name  -> return $ Ident scope (name ++ "_" ++ show u)
    Symbol scope name -> return $ Symbol scope (name ++ "_" ++ show u)


desugar :: Module Ann -> Module Ann
desugar m = evalState (desugarModule m) 0

desugarModule :: Module Ann -> Desugar (Module Ann)
desugarModule (Module scope head pragma imports decls) = do
  decls' <- mapM desugarDecl decls
  return $ Module scope head pragma imports decls'

desugarDecl :: Decl (Scoped SrcSpan) -> Desugar (Decl (Scoped SrcSpan))

desugarDecl (FunBind scope [Match mScope name [pat] (UnGuardedRhs rhsScope rhs) binds]) = do
  a <- newName
  rhs' <- desugarPat a pat rhs
  return $ FunBind scope [Match mScope name [PVar (ann pat) a] (UnGuardedRhs rhsScope rhs') binds]

desugarDecl (PatBind patScope (PVar varScope name) _ rhs binds) =
  desugarDecl (FunBind patScope [Match varScope name [] rhs binds])
desugarDecl decl = return decl


desugarPat :: Name Ann -> Pat Ann -> Exp Ann -> Desugar (Exp Ann)
desugarPat bind pat rest =
  return $
    Case noScope (Var noScope (UnQual noScope bind))
      [Alt noScope pat (UnGuardedAlt noScope rest) Nothing]




{-
fn [] = 0
fn (Cons a b) = a + fn b

==>

fn lst =
  case lst of
    [] -> 0
    Cons a b -> a + fn b
-}


{-

fn (Just (a,b)) = a + b

==>

fn arg =
  case arg of
    Just sub ->
      case sub of
        (,) a b -> a + b

-}

--data Gen a

--mkFuns :: HSE.Decl (Scoped SrcLoc) -> Gen Desugar ()
--mkFuns (HSE.FunBind _scope [HSE.Match _loc name pats rhs _binds]) = return ()
--mkFuns (HSE.PatBind pscope (HSE.PVar scope name) _ty rhs binds) =
--  mkFuns (HSE.FunBind pscope [HSE.Match scope name [] rhs binds])
--mkFuns _ = return ()
--decl -> genesis ()
--Fun name [pat] rhs =
--  desugarPat pat $
--  desugarRhs rhs

{-

newtype New = New Old

fn (New old) = other_fn old

==>

fn arg = other_fn old

-}

{-

Just x = fn

==>

tmp = fn
x = case tmp of Just val -> val; Nothing -> error ...

-}


{-

fn a = \b -> a + b

==>

fn a b = a + b

-}

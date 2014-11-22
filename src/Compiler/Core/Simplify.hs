module Compiler.Core.Simplify where

import Compiler.Core
import Language.Haskell.TypeCheck.Types

simplify :: Module -> Module
simplify m = m
    { coreDecls = map decl (coreDecls m) }
  where

    decl (Decl ty name body) = Decl ty name (expr body)
    expr e =
      case e of
        Var{} -> e
        Con{} -> e
        UnboxedTuple{} -> e
        Lit{} -> e
        WithExternal{} -> e
        App a b -> App (expr a) (expr b)
        Lam a (Lam b rest) -> expr (Lam (a++b) rest)
        Lam vars rest -> Lam vars (expr rest)
        -- Let (NonRec bind rhs) (Var bind') | bind == bind' ->
        --     expr rhs
        Let bind rest -> Let (letBind bind) (expr rest)
        Case scrut alts -> Case (expr scrut) (map alt alts)
        Cast rest ty -> Cast (expr rest) ty
        Id -> e
        WithCoercion (CoerceAp []) rest -> expr rest
        WithCoercion (CoerceAbs []) rest -> expr rest
        WithCoercion coercion rest -> WithCoercion coercion (expr rest)
    alt (Alt pattern branch) = Alt pattern (expr branch)
    letBind (NonRec bind rhs) = NonRec bind (expr rhs)
    letBind (Rec binds) = Rec [ (bind, expr rhs) | (bind, rhs) <- binds ]



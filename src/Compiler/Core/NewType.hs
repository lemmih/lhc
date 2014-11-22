module Compiler.Core.NewType where

import Compiler.Core

lower :: Module -> Module
lower m = m
    { coreDecls = map decl (coreDecls m) }
  where
    isNewtype name = name `elem` [ con | NewType con <- coreNewTypes m ]

    decl (Decl ty name body) = Decl ty name (expr body)
    expr e =
      case e of
        Var{} -> e
        Con con
          | isNewtype con -> Id
          | otherwise     -> e
        UnboxedTuple{} -> e
        Lit{} -> e
        WithExternal{} -> e
        App (Con con) b | isNewtype con -> expr b
        App a b -> App (expr a) (expr b)
        Lam vars rest -> Lam vars (expr rest)
        Let bind rest -> Let (letBind bind) (expr rest)
        Case scrut [Alt (ConPat con [arg]) branch]
          | isNewtype con -> Let (NonRec arg scrut) branch
        Case scrut alts -> Case (expr scrut) (map alt alts)
        Cast rest ty -> Cast (expr rest) ty
        Id -> e
        WithCoercion coercion rest -> WithCoercion coercion (expr rest)
    alt (Alt pattern branch) = Alt pattern (expr branch)
    letBind (NonRec bind rhs) = NonRec bind (expr rhs)
    letBind (Rec binds) = Rec [ (bind, expr rhs) | (bind, rhs) <- binds ]



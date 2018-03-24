module Compiler.Core.NewType where

import Compiler.Core
import Language.Haskell.TypeCheck

lower :: Module -> Module
lower m = m
    { coreDecls = map decl (coreDecls m) }
  where
    isNewtype name = name `elem` [ con | IsNewType con <- coreNewTypes m ]

    decl (Decl ty name body) = Decl ty name (expr body)
    expr e =
      case e of
        Var{} -> e
        -- WithCoercion (CoerceAp tys) (Con con)
        --   | isNewtype con ->
        --       case varType con of
        --         TcForall vars _ | length vars == length tys -> Id
        --         _ -> error $ "Weird newtype: " ++ show e
        Con con
          | isNewtype con -> Id
          | otherwise     -> e
        UnboxedTuple{} -> e
        Lit{} -> e
        WithExternal{} -> e
        ExternalPure{} -> e
        App (Con con) b | isNewtype con -> expr b
        App a b -> App (expr a) (expr b)
        Lam vars rest -> Lam vars (expr rest)
        Let bind rest -> Let (letBind bind) (expr rest)
        LetStrict bind e1 e2 -> LetStrict bind (expr e1) (expr e2)
        Case scrut _ _def [Alt (ConPat con [arg]) branch]
          | isNewtype con -> Let (NonRec arg scrut) (expr branch)
        Case scrut var defaultBranch alts -> Case (expr scrut) var (fmap expr defaultBranch) (map alt alts)
        Cast rest ty -> Cast (expr rest) ty
        Id -> e
        -- WithCoercion coercion rest -> WithCoercion coercion (expr rest)
    alt (Alt pattern branch) = Alt pattern (expr branch)
    letBind (NonRec bind rhs) = NonRec bind (expr rhs)
    letBind (Rec binds) = Rec [ (bind, expr rhs) | (bind, rhs) <- binds ]

module Compiler.Core.Simplify (simplify) where

import Compiler.Core

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
        ExternalPure{} -> e
        App Id b -> expr b
        App a b -> App (expr a) (expr b)
        Lam a (Lam b rest) -> expr (Lam (a++b) rest)
        Lam vars rest -> Lam vars (expr rest)
        Let bind@(NonRec _ Var{}) (Lam a b) ->
            expr $ Lam a (Let bind b)
        Let (NonRec bind rhs) e | (Var bind', apps) <- collectApps e
                                , varName bind == varName bind' ->
            foldl App (expr rhs) apps
        Let bind rest -> Let (letBind bind) (expr rest)
        LetStrict bind e1 e2 -> LetStrict bind (expr e1) (expr e2)
        Case scrut var (Just (Case (Var scrut') var' mbDef alts')) alts | varName var == varName scrut' ->
            expr $ Case scrut var mbDef (alts ++ alts')
        Case scrut var defaultBranch alts ->
            Case (expr scrut) var (fmap expr defaultBranch) (map alt alts)
        Cast rest ty -> Cast (expr rest) ty
        Id -> e
        WithProof _p e -> expr e -- WithProof p (expr e)
        -- WithCoercion (CoerceAp []) rest -> expr rest
        -- WithCoercion (CoerceAbs []) rest -> expr rest
        -- WithCoercion CoerceId rest -> expr rest
        -- WithCoercion coercion rest -> WithCoercion coercion (expr rest)
    alt (Alt pattern branch) = Alt pattern (expr branch)
    letBind (NonRec bind rhs) = NonRec bind (expr rhs)
    letBind (Rec binds) = Rec [ (bind, expr rhs) | (bind, rhs) <- binds ]

collectApps :: Expr -> (Expr, [Expr])
collectApps = worker []
  where
    worker acc (App a b) = worker (b:acc) a
    worker acc other = (other, reverse acc)

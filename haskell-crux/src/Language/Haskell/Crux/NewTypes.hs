module Language.Haskell.Crux.NewTypes where

import           Language.Haskell.Crux

lowerNewTypes :: Module -> Module
lowerNewTypes m = m
    { cruxDecls = map decl (cruxDecls m) }
  where
    decl (Declaration ty name body) = Declaration ty name (expr body)
    expr e =
      case e of
        Var{} -> e
        Con{} -> e
        UnboxedTuple{} -> e
        Lit{} -> e
        WithExternal out outS external args st rest ->
          WithExternal out outS external (map expr args) (expr st) $ expr rest
        ExternalPure out external args rest ->
          ExternalPure out external (map expr args) $ expr rest
        App a b -> App (expr a) (expr b)
        Lam vars rest -> Lam vars (expr rest)
        Let bind rest -> Let (letBind bind) (expr rest)
        LetStrict bind e1 e2 -> LetStrict bind (expr e1) (expr e2)
        Case scrut _var Nothing [Alt (ConPat con [unpacked]) branch]
          | IsNewType con `elem` cruxNewTypes m ->
          Let (NonRec unpacked (App Cast scrut)) branch
        Case scrut var defaultBranch alts ->
            Case (expr scrut) var (fmap expr defaultBranch) (map alt alts)
        Convert rest ty -> Convert (expr rest) ty
        Cast -> Cast
    alt (Alt p branch) = Alt p (expr branch)
    letBind (NonRec bind rhs) = NonRec bind (expr rhs)
    letBind (Rec binds) = Rec [ (bind, expr rhs) | (bind, rhs) <- binds ]

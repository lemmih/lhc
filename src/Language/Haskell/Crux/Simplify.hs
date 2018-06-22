module Language.Haskell.Crux.Simplify (simplify) where

import Language.Haskell.Crux

simplify :: Module -> Module
simplify m = m
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
        App Id b -> expr b
        App (Lam (v:vs) body) b ->
          expr (Let (NonRec v b) (Lam vs body))
        App (Let bind rest) b ->
          expr (Let bind (App rest b))
        App a (ExternalPure out external args rest) ->
          expr $ ExternalPure out external args $
                 App a rest
        App a b -> App (expr a) (expr b)
        Lam [] rest -> expr rest
        Lam a (Lam b rest) -> expr (Lam (a++b) rest)
        Lam vars rest -> Lam vars (expr rest)
        Let bind@(NonRec _ Var{}) (Lam a b) ->
            expr $ Lam a (Let bind b)
        Let (NonRec bind rhs) e' | (Var bind', apps) <- collectApps e'
                                 , varName bind == varName bind' ->
            foldl App (expr rhs) apps
        Let bind rest -> Let (letBind bind) (expr rest)
        LetStrict bind e1 e2 -> LetStrict bind (expr e1) (expr e2)
        Case (LetStrict bind e1 e2) var mbDef alts ->
          expr $ LetStrict bind e1 $ Case e2 var mbDef alts
        Case (Let (NonRec bind e1) e2) var mbDef alts ->
          expr $ Let (NonRec bind e1) $ Case e2 var mbDef alts
        Case (Case scrut subVar subDef [Alt pat branch]) var mbDef alts ->
          expr $ Case scrut subVar subDef [Alt pat $ Case branch var mbDef alts]
        Case (ExternalPure out external args rest) var mbDef alts ->
          expr $ ExternalPure out external args $
            Case rest var mbDef alts
        Case (WithExternal out outS external args st rest) var mbDef alts ->
          expr $ WithExternal out outS external args st $
                 Case rest var mbDef alts
        Case (UnboxedTuple es) _var Nothing [Alt (UnboxedPat vs) branch] ->
          expr $ foldr (\(v,e') -> Let (NonRec v e')) branch (zip vs es)
        Case scrut var (Just (Case (Var scrut') _var' mbDef alts')) alts | varName var == varName scrut' ->
            expr $ Case scrut var mbDef (alts ++ alts')
        Case scrut var defaultBranch alts ->
            Case (expr scrut) var (fmap expr defaultBranch) (map alt alts)
        Convert rest ty -> Convert (expr rest) ty
        Id -> e
    alt (Alt p branch) = Alt p (expr branch)
    letBind (NonRec bind rhs) = NonRec bind (expr rhs)
    letBind (Rec binds) = Rec [ (bind, expr rhs) | (bind, rhs) <- binds ]

collectApps :: Expr -> (Expr, [Expr])
collectApps = worker []
  where
    worker acc (App a b) = worker (b:acc) a
    worker acc other = (other, reverse acc)

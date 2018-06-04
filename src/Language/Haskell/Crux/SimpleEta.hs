module Language.Haskell.Crux.SimpleEta
  ( SimpleEtaAnnotation
  , simpleEta
  , emptySimpleEtaAnnotation
  ) where

import           Language.Haskell.Crux
import           Language.Haskell.Crux.FreeVariables

import           Control.Monad.Reader
import           Control.Monad.Writer                (WriterT (..), tell)
import           Data.Graph                          (SCC (..),
                                                      stronglyConnComp)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import qualified Data.Set                            as Set

-- 1. order by scc
-- 2. do eta abstraction on top-level defs and for lambdas
-- 3. record the new number of arguments for top-levels.

type SimpleEtaAnnotation = Map Name [Variable]
emptySimpleEtaAnnotation :: SimpleEtaAnnotation
emptySimpleEtaAnnotation = Map.empty

simpleEta :: SimpleEtaAnnotation -> Module -> (SimpleEtaAnnotation, Module)
simpleEta anns0 m =
    case worker anns0 [] scc of
      (anns, decls) -> (anns, m{cruxDecls = decls})
  where
    graph =
          [ (decl, declName decl, Set.toList free)
          | decl <- cruxDecls m
          , let free = freeVariablesDecl decl ]
    scc = stronglyConnComp graph
    worker anns lst [] = (anns, reverse lst)
    worker anns lst (group:groups) =
      case group of
        AcyclicSCC decl ->
          let (args, expr) = etaTopLevel anns (declBody decl)
              decl' = decl{declBody = expr}
              anns' = Map.insert (declName decl) args anns
          in worker anns' (decl':lst) groups
        CyclicSCC decls ->
          worker anns lst (map AcyclicSCC decls ++ groups)



type M a = WriterT Out (Reader Env) a
type Env = (SimpleEtaAnnotation, [Variable])
data Out = Empty
         | Limit [Variable]

instance Semigroup Out where
  Empty <> b             = b
  a <> Empty             = a
  Limit a <> Limit b     = Limit (zipWith const a b)

instance Monoid Out where
  mempty = Empty



etaTopLevel :: SimpleEtaAnnotation -> Expr -> ([Variable], Expr)
-- etaTopLevel anns (WithCoercion e (Lam vars expr)) =
--   let (vars', expr') = runM anns (etaExpr expr)
--   in (vars ++ vars', WithCoercion e (Lam (vars ++ vars') expr'))
etaTopLevel anns (Lam vars expr) =
  let (vars', expr') = runM anns (etaExpr expr)
  in (vars ++ vars', Lam (vars ++ vars') expr')
etaTopLevel anns expr =
  let (vars, expr') = runM anns (etaExpr expr)
  in (vars, if null vars then expr' else Lam vars expr')


runM :: SimpleEtaAnnotation -> M a -> ([Variable], a)
runM anns action =
    (vars, a)
  where
    (a, out) = runReader (runWriterT action) env
    vars = case out of Empty -> []; Limit lst -> lst
    env = (anns, vars)

lookupArguments :: Name -> M [Variable]
lookupArguments fn = asks $ \(anns, _) ->
  Map.findWithDefault [] fn anns



etaExpr :: Expr -> M Expr
etaExpr expr =
  case expr of
    _ | Just (fn, args) <- collectApp expr -> do
      fnArgs <- lookupArguments (varName fn)
      let newArgs = drop (length args) fnArgs
      tell (Limit newArgs)
      etaArguments $ foldl App expr (map Var newArgs)
    Con{} -> pure expr
    UnboxedTuple{} -> pure expr
    Lit{} -> pure expr
    -- WithCoercion c e -> WithCoercion c <$> etaExpr e

    -- WithExternal Variable String [Variable] Variable Expr
    -- ExternalPure Variable String [Variable] Expr
    -- Lam [Variable] Expr
    -- Let LetBind Expr
    -- LetStrict Variable Expr Expr
    Case scrut thunk mbDefault alts ->
      Case scrut thunk
        <$> etaMaybe etaExpr mbDefault
        <*> mapM etaAlt alts
    -- Cast Expr TcType
    -- Id
    -- WithCoercion Coercion Expr

    _ -> etaArguments expr

etaArguments :: Expr -> M Expr
etaArguments expr =
  case expr of
    App a b -> App <$> etaArguments a <*> etaArgument b
    _       -> pure expr

etaArgument :: Expr -> M Expr
etaArgument expr =
  case expr of
    Lam vars e -> do
      (anns, _) <- ask
      let (vars', e') = runM anns (etaExpr e)
      pure $ Lam (vars ++ vars') e'
    App a b -> App <$> etaArgument a <*> etaArgument b
    Case scrut thunk mbDefault alts ->
      Case <$> etaArgument scrut
           <*> pure thunk
           <*> etaMaybe etaArgument mbDefault
           <*> sequence [ Alt pattern <$> etaArgument e | Alt pattern e <- alts ]
    _ -> pure expr

etaMaybe :: (a -> M a) -> Maybe a -> M (Maybe a)
etaMaybe _ Nothing   = pure Nothing
etaMaybe fn (Just v) = Just <$> fn v

etaAlt :: Alt -> M Alt
etaAlt (Alt pattern e) =
  Alt pattern <$> etaExpr e

collectApp :: Expr -> Maybe (Variable, [Expr])
collectApp = worker []
  where
    worker acc expr =
      case expr of
        Var v   -> Just (v, reverse acc)
        App a b -> worker (b:acc) a
        -- WithCoercion _ e -> worker acc e
        _       -> Nothing

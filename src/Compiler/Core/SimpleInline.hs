module Compiler.Core.SimpleInline
  ( simpleInline ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Graph
import           Control.Monad.Reader
import           Control.Monad.Writer        ( Writer, runWriter, tell, censor )

import           Compiler.Core
import           Compiler.Core.FreeVariables
import           Data.Bedrock                (Name)

-- SCC
-- inline everything & analyse whether current function can be inlined later


-- Functions can be inlineable iff:
--  They do not refer to any Haskell functons.
--  They only use their arguments once.
-- Functions can be inlined iff:
--  They are called with all arguments supplied.

simpleInline :: Module -> Module
simpleInline m =
    m{coreDecls = worker Map.empty flatDecls}
  where
    graph =
          [ (decl, declName decl, Set.toList free)
          | decl <- coreDecls m
          , let free = freeVariablesDecl decl ]
    scc = stronglyConnComp graph
    flatDecls = flattenSCCs scc
    worker env [] = []
    worker env (decl:decls) =
      let (decl', env') = inlineDecl env decl
      in decl' : worker env' decls

data Usage
  = Unuable
  | Using (Set Name)
  deriving (Show)

instance Monoid Usage where
  mempty = Using Set.empty
  mappend Unuable b = Unuable
  mappend a Unuable = Unuable
  mappend (Using a) (Using b)
    | Set.null (a `Set.intersection` b) = Using (Set.union a b)
    | otherwise                       = Unuable

type Env = Map Name ([Variable], Expr)

type M a = ReaderT Env (Writer Usage) a

inlineDecl :: Env -> Decl -> (Decl, Env)
inlineDecl env decl = (decl', env')
  where
    (body, usage) = runWriter (runReaderT (inlineExpr (declBody decl)) env)
    decl' = decl{declBody = body}
    env' = case usage of
      Unuable  -> env
      Using vars
        | Set.null vars -> Map.insert (declName decl) (splitArguments body) env
        | otherwise     -> env

splitArguments :: Expr -> ([Variable], Expr)
splitArguments (Lam vars e) = (vars, e)
-- splitArguments (WithCoercion _ e) = splitArguments e
splitArguments e = ([], e)

inlineExpr :: Expr -> M Expr
inlineExpr expr =
  case expr of
    Con{} -> pure expr
    UnboxedTuple args ->
      UnboxedTuple <$> mapM inlineExpr args
    -- WithExternal Variable String [Variable] Variable Expr
    -- ExternalPure Variable String [Variable] Expr
    -- Lam [] e -> inlineExpr e
    -- App (Lam (v:vs) body) e ->
    --   replace (varName v) e (inlineExpr $ Lam vs body)
    Lam vars e ->
      Lam vars <$> dropUsages (map varName vars) (inlineExpr e)
    -- Let LetBind Expr
    -- LetStrict Variable Expr Expr
    Case e scrut mbDef alts ->
      Case <$> inlineExpr e
           <*> pure scrut
           <*> dropUsage (varName scrut) (inlineMaybe inlineExpr mbDef)
           <*> dropUsage (varName scrut) (mapM inlineAlt alts)
    Cast e ty ->
      Cast <$> inlineExpr e <*> pure ty
    -- WithCoercion c e ->
    --   WithCoercion c <$> inlineExpr e
    _ | (Lam vars e, args) <- collectApp expr -> do
      let extraArgs = drop (length vars) args
      replaceMany (zip (map varName vars) args) $ do
        e' <- inlineExpr e
        pure $ foldl App e' extraArgs
    _ | (Var var, args) <- collectApp expr -> do
      mbInline <- lookupInline (varName var)
      case mbInline of
        Nothing -> do
          tell $ Using $ Set.singleton (varName var)
          args <- mapM inlineExpr args
          pure $ foldl App (Var var) args
        Just (fnArgs, fnBody)
          | extraArgs <- drop (length fnArgs) args
          , length fnArgs <= length args ->
            replaceMany (zip (map varName fnArgs) args) $
            -- noReplace (varName var) $
            inlineExpr (foldl App fnBody extraArgs)
          | otherwise -> do
            tell $ Using $ Set.singleton (varName var)
            args <- mapM inlineExpr args
            pure $ foldl App (Var var) args
    App a b ->
      App <$> inlineExpr a <*> inlineExpr b
    _ -> do
      tell Unuable
      pure expr

inlineAlt :: Alt -> M Alt
inlineAlt (Alt pattern e) =
  case pattern of
    ConPat _name vars -> dropUsages (map varName vars) $
      Alt pattern <$> inlineExpr e
    LitPat{} ->
      Alt pattern <$> inlineExpr e
    UnboxedPat vars ->
      dropUsages (map varName vars) $
      Alt pattern <$> inlineExpr e



lookupInline :: Name -> M (Maybe ([Variable], Expr))
lookupInline var = asks $ Map.lookup var

dropUsage :: Name -> M a -> M a
dropUsage var = censor fn
  where
    fn Unuable = Unuable
    fn (Using lst) = Using $ Set.delete var lst

dropUsages :: [Name] -> M a -> M a
dropUsages vars = censor fn
  where
    fn Unuable = Unuable
    fn (Using lst) = Using $ lst `Set.difference` Set.fromList vars

replace :: Name -> Expr -> M a -> M a
replace v e = local $ Map.insert v ([], e)

replaceMany :: [(Name, Expr)] -> M a -> M a
replaceMany [] = id
replaceMany ((v,e):xs) = replace v e . replaceMany xs

-- noReplace :: Name -> M a -> M a
-- noReplace v = local $ Map.delete v

-- noReplaceMany :: [Name] -> M a -> M a
-- noReplaceMany [] = id
-- noReplaceMany (v:vs) = noReplace v . noReplaceMany vs

collectApp :: Expr -> (Expr, [Expr])
collectApp = worker []
  where
    worker acc expr =
      case expr of
        App a b -> worker (b:acc) a
        -- WithCoercion _ e -> worker acc e
        _ -> (expr, acc)

inlineMaybe :: (a -> M a) -> Maybe a -> M (Maybe a)
inlineMaybe _ Nothing   = pure Nothing
inlineMaybe fn (Just v) = Just <$> fn v

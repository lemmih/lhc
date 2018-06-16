module Language.Haskell.Crux.SimpleInline
  ( simpleInline ) where

import           Control.Monad.Reader
import           Control.Monad.RWS                   (RWS, evalRWS)
import           Control.Monad.State                 (gets, modify)
import           Control.Monad.Writer                (listen, tell)
import           Data.Array
import           Data.Graph
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Semigroup
import           Data.Set                            (Set)
import qualified Data.Set                            as Set

import           Language.Haskell.Crux
import           Language.Haskell.Crux.FreeVariables

-- SCC
-- inline everything & analyse whether current function can be inlined later


-- Functions can be inlineable iff:
--  They do not refer to any Haskell functons.
--  They only use their arguments once.
--  They are only used once.
-- Functions can be inlined iff:
--  They are called with all arguments supplied.

simpleInline :: Module -> Module
simpleInline m =
    m{cruxDecls = fst (evalRWS (mapM worker flatDecls) emptyEnv emptyState) }
  where
    emptyEnv = Map.empty
    emptyState = State 1 Map.empty Set.empty
    (graph, fromVertex) = graphFromEdges'
          [ (decl, declName decl, Set.toList free)
          | decl <- cruxDecls m
          , let free = freeVariablesDecl decl ]
    graph' = transposeG graph
    isOneShot v =
      case graph'!v of
        []  -> True
        [_] -> True
        _   -> False
    flatDecls = topSort graph'
    worker v = do
      let (decl,_,_) = fromVertex v
          name = declName decl
          (args, body) = splitArguments (declBody decl)
      (decl', usage) <- listen (uniqueDecl decl)
      when (mayInline usage (isOneShot v)) $
        modify $ \st ->
          st{ stInline = Map.insert name (args, body) (stInline st) }
      pure decl'

mayInline :: Cost -> Bool -> Bool
mayInline (Cheap n) _isOneShot | n < 20 = True
mayInline Expensive True       = True
mayInline _ _                  = False

splitArguments :: Expr -> ([Variable], Expr)
splitArguments (Lam vars e) = (vars, e)
-- splitArguments (WithCoercion _ e) = splitArguments e
splitArguments e            = ([], e)


type Env = Map Name Expr
data State = State
  { stUnique :: Int
  , stInline :: Map Name ([Variable], Expr)
  , stCheap  :: Set Name
  }
data Cost = Prohibitive | Expensive | Cheap Int deriving (Eq,Show)

instance Semigroup Cost where
  Cheap a <> Cheap b = Cheap (a+b)
  _ <> Prohibitive = Prohibitive
  Prohibitive <> _ = Prohibitive
  _ <> _ = Expensive

instance Monoid Cost where
  mempty = Cheap 0
  mappend = (<>)
type M a = RWS Env Cost State a

uniqueDecl :: Declaration -> M Declaration
uniqueDecl decl = do
  body <- uniqueExpr (declBody decl)
  return decl{declBody = body}

uniqueExpr :: Expr -> M Expr
uniqueExpr expr = do
  tell (Cheap 1)
  case expr of
    _ | (Var var, args) <- collectApp expr -> do
      inline <- gets stInline
      case Map.lookup (varName var) inline of
        Nothing -> do
          core <- uniqueVariable var
          foldl App core <$> mapM uniqueExpr args
        Just (fnArgs, fnBody)
          | length fnArgs <= length args -> do
            args' <- mapM uniqueExpr args
            replaceMany (zip fnArgs args') $
              uniqueExpr (foldl App fnBody (drop (length fnArgs) args'))
          | otherwise -> do
            core <- uniqueVariable var
            foldl App core <$> mapM uniqueExpr args
    Var var -> uniqueVariable var
    Con name -> pure $ Con name
    UnboxedTuple args ->
      UnboxedTuple <$> mapM uniqueExpr args
    Lit lit -> pure $ Lit lit
    WithExternal out retS fn args st e ->
      bind out $ \out' ->
      bind retS $ \retS' ->
      WithExternal out' retS' fn
        <$> mapM uniqueExpr args
        <*> uniqueExpr st
        <*> uniqueExpr e
    ExternalPure out fn args e ->
      bind out $ \out' ->
        ExternalPure out' fn
          <$> mapM uniqueExpr args
          <*> uniqueExpr e
    App a b -> App <$> uniqueExpr a <*> uniqueExpr b
    Lam vars e -> bindMany vars $ \vars' ->
      Lam vars' <$> uniqueExpr e
    Let (NonRec v e) body -> bind v $ \v' ->
      Let <$> (NonRec v' <$> uniqueExpr e)
          <*> uniqueExpr body
    Let{} ->
      error "Compiler.Core.Unique.uniqueExpr.Let: undefined"
    LetStrict v e rest ->
      bind v $ \v' ->
        LetStrict v' <$> uniqueExpr e <*> uniqueExpr rest
    Case e scrut mbDef alts -> do
      e' <- uniqueExpr e
      bind scrut $ \scrut' ->
        Case
          <$> pure e'
          <*> pure scrut'
          <*> uniqueMaybe uniqueExpr mbDef
          <*> mapM uniqueAlt alts
    Cast e ty ->
      Cast <$> uniqueExpr e <*> pure ty
    Id -> pure Id
    WithProof p e -> WithProof p <$> uniqueExpr e

uniqueAlt :: Alt -> M Alt
uniqueAlt (Alt pattern e) =
  case pattern of
    ConPat con vars -> bindMany vars $ \vars' ->
      Alt (ConPat con vars') <$> uniqueExpr e
    LitPat lit ->
      Alt (LitPat lit) <$> uniqueExpr e
    UnboxedPat vars -> bindMany vars $ \vars' ->
      Alt (UnboxedPat vars') <$> uniqueExpr e

uniqueVariable :: Variable -> M Expr
uniqueVariable var@(Variable name _ty) = do
  env <- ask
  case Map.lookup name env of
    Nothing -> do
      consume var
      pure (Var var)
    Just expr -> pure expr
    -- Just expr -> do
    --   local (Map.delete name) $ uniqueExpr expr

collectApp :: Expr -> (Expr, [Expr])
collectApp = worker []
  where
    worker acc expr =
      case expr of
        App a b -> worker (b:acc) a
        -- WithCoercion _ e -> worker acc e
        _       -> (expr, acc)

consume :: Variable -> M ()
consume var = do
  cheap <- gets stCheap
  unless (varName var `Set.member` cheap) $ tell Prohibitive
  modify $ \st -> st{ stCheap = Set.delete (varName var) (stCheap st) }

bind :: Variable -> (Variable -> M a) -> M a
bind var action = do
  name' <- newName (varName var)
  let var' = var{varName = name'}
  modify $ \st -> st{ stCheap = Set.insert name' (stCheap st) }
  local
    (Map.insert (varName var) (Var var'))
    (action var')

bindMany :: [Variable] -> ([Variable] -> M a) -> M a
bindMany vars action = worker [] vars
  where
    worker acc []     = action (reverse acc)
    worker acc (x:xs) = bind x $ \x' -> worker (x':acc) xs

replace :: Variable -> Expr -> M a -> M a
replace old new = local (Map.insert (varName old) new)

replaceMany :: [(Variable, Expr)] -> M a -> M a
replaceMany vars action = worker vars
  where
    worker []             = action
    worker ((key,val):xs) = replace key val $ worker xs


-- newVariable :: Variable -> M Variable
-- newVariable v = do
--   u <- get
--   put (u+1)
--   return v{varName = (varName v){nameUnique = u}}

newName :: Name -> M Name
newName n = do
  u <- gets stUnique
  modify $ \st -> st{stUnique = (u+1)}
  return n{nameUnique = u}

uniqueMaybe :: (a -> M a) -> Maybe a -> M (Maybe a)
uniqueMaybe _ Nothing   = pure Nothing
uniqueMaybe fn (Just v) = Just <$> fn v


{-
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
-}

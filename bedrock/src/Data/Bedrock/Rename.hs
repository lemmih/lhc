{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE BangPatterns               #-}
module Data.Bedrock.Rename (unique, locallyUnique) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List            (elemIndex)
import           Data.Map             (Map)
import qualified Data.Map             as Map

import           Data.Bedrock
import           Data.Bedrock.Misc

type Env = Map Name Name

newtype Uniq a = Uniq { unUniq :: ReaderT Env (State AvailableNamespace) a }
    deriving ( Monad, MonadReader Env, MonadState AvailableNamespace
             , Functor, Applicative )

unique :: Module -> Module
unique m = evalState (runReaderT (unUniq (uniqModule m)) env) st
  where
    env = Map.empty
    st = AvailableNamespace
        { nsNextPointerId = 0
        , nsNextNodeId = 0
        , nsNextPrimitiveId = 0
        , nsNextGlobalId = 0 }



newUnique :: Maybe Type -> Uniq Int
newUnique mbTy = do
    ns <- get
    let (idNum, ns') = newGlobalID ns
    -- let (idNum, ns') = case mbTy of
    --         Nothing -> newGlobalID ns
    --         Just ty -> newIDByType ns ty
    put ns'
    return idNum

newName :: Maybe Type -> Name -> Uniq Name
newName mbTy name = do
    u <- newUnique mbTy
    return name{ nameUnique = u }

rename :: Maybe Type -> Name -> Uniq a -> Uniq a
rename mbTy old action = do
    new <- newName mbTy old
    local (Map.insert old new) action

renameAll :: [Name] -> Uniq a -> Uniq a
renameAll xs action = foldr (rename Nothing) action xs

renameVariables :: [Variable] -> Uniq a -> Uniq a
renameVariables [] action     = action
renameVariables (v:vs) action =
    rename Nothing (variableName v) $
    renameVariables vs action

resolveName :: Name -> Uniq Name
resolveName name = do
  m <- ask
  case Map.lookup name m of
    -- Nothing  -> pure $ name{ nameModule = nameModule name ++ [nameIdentifier name], nameIdentifier = "unresolved"}
    Nothing  -> error $ "Unresolved identifier: " ++ show (name, Map.keys m)
    Just new -> return new

resolveNodeName :: NodeName -> Uniq NodeName
resolveNodeName nodeName =
    case nodeName of
        ConstructorName name blanks ->
            ConstructorName <$> resolveName name <*> pure blanks
        FunctionName name blanks ->
            FunctionName <$> resolveName name <*> pure blanks
        UnboxedTupleName -> pure UnboxedTupleName
        --CatchFrame name blanks ->
        --    CatchFrame <$> resolveName name <*> pure blanks

resolve :: Variable -> Uniq Variable
resolve var = do
  name <- resolveName (variableName var)
  pure var{ variableName = name }

--resolveArgument :: Argument -> Uniq Argument
--resolveArgument arg =
--    case arg of
--        RefArg var            ->
--            RefArg <$> resolve var
--        LitArg lit            ->
--            pure (LitArg lit)
--        NodeArg nodeName vars ->
--            NodeArg <$> resolveNodeName nodeName <*> mapM resolve vars

uniqModule :: Module -> Uniq Module
uniqModule m =
    renameAll [ name | NodeDefinition name _args <- nodes m ] $
    renameAll (map fnName (functions m)) $ do
        ns  <- mapM uniqNode (nodes m)
        fns <- mapM uniqFunction (functions m)
        entry <- resolveName (entryPoint m)
        namespace <- get
        return Module
            { modForeigns = modForeigns m
            , nodes = ns
            , entryPoint = entry
            , functions = fns
            , modNamespace = namespace
            }

uniqNode :: NodeDefinition -> Uniq NodeDefinition
uniqNode (NodeDefinition name args) =
    NodeDefinition <$> resolveName name <*> pure args

uniqFunction :: Function -> Uniq Function
uniqFunction (Function name attrs args rets body) = renameVariables args $
    Function
        <$> resolveName name
        <*> mapM uniqAttribute attrs
        <*> mapM resolve args
        <*> pure rets
        <*> uniqBlock body

uniqAttribute :: Attribute -> Uniq Attribute
uniqAttribute NoCPS              = pure NoCPS
uniqAttribute Internal           = pure Internal
uniqAttribute (AltReturn n name) = AltReturn n <$> resolveName name
uniqAttribute (Prefix size prim ptrs (Just name)) = Prefix size prim ptrs <$> (Just <$> resolveName name)
uniqAttribute attr@Prefix{} = pure attr

uniqBlock :: Block -> Uniq Block
uniqBlock block =
    case block of
        Case scrut mbBranch alts ->
            Case
                <$> resolve scrut
                <*> uniqMaybe uniqBlock mbBranch
                <*> mapM uniqAlternative alts
        Bind binds simple rest -> renameVariables binds $
            Bind
                <$> mapM resolve binds
                <*> uniqSimple simple
                <*> uniqBlock rest
        Return vars ->
            Return <$> mapM resolve vars
        Raise var ->
            Raise <$> resolve var
        TailCall fn vars ->
            TailCall <$> resolveName fn <*> mapM resolve vars
        Invoke n fn vars ->
            Invoke n <$> resolve fn <*> mapM resolve vars
        -- InvokeHandler cont exception ->
        --     InvokeHandler <$> resolve cont <*> resolve exception
        Exit -> pure Exit
        Panic msg -> pure (Panic msg)

uniqAlternative :: Alternative -> Uniq Alternative
uniqAlternative (Alternative pattern branch) =
    case pattern of
        LitPat{} -> Alternative pattern <$> uniqBlock branch
        NodePat nodeName vars ->
            renameVariables vars $
                Alternative
                    <$> (NodePat
                        <$> resolveNodeName nodeName
                        <*> mapM resolve vars)
                    <*> uniqBlock branch
        -- VarPat var ->
        --     renameVariables [var] $
        --         Alternative
        --             <$> (VarPat <$> resolve var)
        --             <*> uniqBlock branch
        -- UnboxedPat vars ->
        --     renameVariables vars $
        --         Alternative
        --             <$> (UnboxedPat <$> mapM resolve vars)
        --             <*> uniqBlock branch


uniqMaybe :: (a -> Uniq a) -> Maybe a -> Uniq (Maybe a)
uniqMaybe fn obj =
    case obj of
        Nothing  -> return Nothing
        Just val -> Just <$> fn val

uniqParameter :: Parameter -> Uniq Parameter
uniqParameter param =
  case param of
    PInt{} -> pure param
    PString{} -> pure param
    PName name -> PName <$> resolveName name
    PNodeName node -> PNodeName <$> resolveNodeName node
    PVariable var -> PVariable <$> resolve var
    PVariables vars -> PVariables <$> mapM resolve vars

uniqSimple :: Expression -> Uniq Expression
uniqSimple simple =
  case simple of
    Application fn vars ->
        Application <$> resolveName fn <*> mapM resolve vars
    CCall fn vars ->
        CCall fn <$> mapM resolve vars
    Catch exh exhArgs fn fnArgs ->
        Catch
            <$> resolveName exh <*> mapM resolve exhArgs
            <*> resolveName fn <*> mapM resolve fnArgs
    InvokeReturn n fn vars ->
      InvokeReturn n <$> resolve fn <*> mapM resolve vars
    Builtin fn params ->
      Builtin fn <$> mapM uniqParameter params
    Literal lit -> pure $ Literal lit







---------------------------------------------------------
-- Locally Unique

type Scope = Map Name [Name]
data LUniq a = LUniq {unLUniq :: Scope -> (a, Scope)}

instance Monad LUniq where
    return x = LUniq $ \scope -> (x,mempty)
    f >>= g = LUniq $ \scope ->
        let (a, scope') = unLUniq f scope
            (b, scope'') = unLUniq (g a) scope
        in (b, Map.unionWith (++) scope' scope'')

instance Functor LUniq where
  fmap fn action = action >>= (return . fn)

instance Applicative LUniq where
  pure = return
  fn <*> action = do
    fn' <- fn
    a <- action
    return (fn' a)

locallyAsk :: LUniq Scope
locallyAsk = LUniq $ \scope -> (scope, mempty)

locallyBind :: Name -> LUniq Name
locallyBind name = do
  LUniq $ \_ -> ((), Map.singleton name{nameUnique=0} [name])
  locallyResolve name

runLUniq :: LUniq a -> a
runLUniq action = a
  where
    (a, scope) = unLUniq action scope

limitScope :: LUniq a -> LUniq a
limitScope action = LUniq $ \scope ->
  let (a, defs) = unLUniq action (Map.unionWith (++) scope defs)
  in (a, mempty)

locallyUnique :: Module -> Module
locallyUnique m = runLUniq $ do
  ns <- mapM locallyNode (nodes m)
  entry <- locallyResolve (entryPoint m)
  fns <- mapM locallyFunction (functions m)
  return m
    { nodes = ns
    , entryPoint = entry
    , functions = fns }

locallyNode :: NodeDefinition -> LUniq NodeDefinition
locallyNode (NodeDefinition name ty) =
  NodeDefinition <$> locallyBind name <*> pure ty

locallyResolve :: Name -> LUniq Name
locallyResolve name = do
  scope <- locallyAsk
  return $
    case elemIndex name =<< Map.lookup name{nameUnique=0} scope of
      Nothing  -> error $ "Data.Bedrock.Rename.locallyResolve: " ++ show name
      Just nth -> name{nameUnique=nth}

locallyResolveVariable :: Variable -> LUniq Variable
locallyResolveVariable (Variable name ty) =
  Variable <$> locallyResolve name <*> pure ty

locallyBindVariable :: Variable -> LUniq Variable
locallyBindVariable (Variable name ty) =
  Variable <$> locallyBind name <*> pure ty

locallyFunction :: Function -> LUniq Function
locallyFunction fn = do
  name <- locallyBind (fnName fn)
  limitScope $ do
    args <- mapM locallyBindVariable (fnArguments fn)
    body <- locallyBlock (fnBody fn)
    attrs <- mapM locallyAttribute (fnAttributes fn)
    return $ fn
      { fnName = name
      , fnArguments = args
      , fnAttributes = attrs
      , fnBody = body }

locallyMaybe :: (a -> LUniq a) -> Maybe a -> LUniq (Maybe a)
locallyMaybe _ Nothing   = return Nothing
locallyMaybe fn (Just a) = Just <$> fn a

locallyAttribute :: Attribute -> LUniq Attribute
locallyAttribute =
  \case
    NoCPS -> pure NoCPS
    Internal -> pure Internal
    AltReturn n name -> AltReturn n <$> locallyResolve name
    Prefix size prim ptrs mbName -> Prefix size prim ptrs <$> locallyMaybe locallyResolve mbName

locallyBlock :: Block -> LUniq Block
locallyBlock block =
  case block of
    Case scrut mbDefault alts ->
      Case
        <$> locallyResolveVariable scrut
        <*> locallyMaybe locallyBlock mbDefault
        <*> mapM locallyAlternative alts
    Bind binds expr rest ->
      Bind
        <$> mapM locallyBindVariable binds
        <*> locallyExpression expr
        <*> locallyBlock rest
    Return args ->
      Return <$> mapM locallyResolveVariable args
    Raise exception ->
      Raise <$> locallyResolveVariable exception
    TailCall fn args ->
      TailCall
        <$> locallyResolve fn
        <*> mapM locallyResolveVariable args
    Invoke n fn args ->
      Invoke n
        <$> locallyResolveVariable fn
        <*> mapM locallyResolveVariable args
    Exit -> return block
    Panic{} -> return block
    -- InvokeHandler{} -> return block

locallyAlternative :: Alternative -> LUniq Alternative
locallyAlternative (Alternative pattern branch) = -- limitScope $
  Alternative
    <$> locallyPattern pattern
    <*> locallyBlock branch

locallyPattern :: Pattern -> LUniq Pattern
locallyPattern pattern =
  case pattern of
    NodePat name args ->
      NodePat
        <$> locallyNodeName name
        <*> mapM locallyBindVariable args
    LitPat{} -> pure pattern

locallyNodeName :: NodeName -> LUniq NodeName
locallyNodeName node =
  case node of
    ConstructorName name missing ->
      ConstructorName
        <$> locallyResolve name
        <*> pure missing
    FunctionName name missing ->
      FunctionName
        <$> locallyResolve name
        <*> pure missing
    UnboxedTupleName -> pure node


locallyParameter :: Parameter -> LUniq Parameter
locallyParameter param =
  case param of
    PInt{} -> pure param
    PString{} -> pure param
    PName name -> PName <$> locallyResolve name
    PNodeName node -> PNodeName <$> locallyNodeName node
    PVariable var -> PVariable <$> locallyResolveVariable var
    PVariables vars -> PVariables <$> mapM locallyResolveVariable vars

locallyExpression :: Expression -> LUniq Expression
locallyExpression expr =
  case expr of
    Application fn args ->
      Application
        <$> locallyResolve fn
        <*> mapM locallyResolveVariable args
    CCall fn args ->
      CCall fn
        <$> mapM locallyResolveVariable args
    Catch fn args handler handlerArgs ->
      Catch fn
        <$> mapM locallyResolveVariable args
        <*> pure handler
        <*> mapM locallyResolveVariable handlerArgs
    InvokeReturn n fn args ->
      InvokeReturn n
        <$> locallyResolveVariable fn
        <*> mapM locallyResolveVariable args
    Builtin fn params ->
      Builtin fn <$> mapM locallyParameter params
    Literal{} -> pure expr

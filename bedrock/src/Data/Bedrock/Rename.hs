{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Bedrock.Rename (unique, locallyUnique) where

import           Control.Applicative  (Applicative, pure, (<$>), (<*>))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List            (elemIndex)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Monoid

import           Data.Bedrock
import           Data.Bedrock.Misc

type Env = Map (Maybe Type, Name) Name

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
    let (idNum, ns') = case mbTy of
            Nothing -> newGlobalID ns
            Just ty -> newIDByType ns ty
    put ns'
    return idNum

newName :: Maybe Type -> Name -> Uniq Name
newName mbTy name = do
    u <- newUnique mbTy
    return name{ nameUnique = u }

rename :: Maybe Type -> Name -> Uniq a -> Uniq a
rename mbTy old action = do
    new <- newName mbTy old
    local (Map.insert (mbTy, old) new) action

renameAll :: [Name] -> Uniq a -> Uniq a
renameAll xs action = foldr (rename Nothing) action xs

renameVariables :: [Variable] -> Uniq a -> Uniq a
renameVariables [] action     = action
renameVariables (v:vs) action =
    rename (Just (variableType v)) (variableName v) $
    renameVariables vs action

resolveName :: Name -> Uniq Name
resolveName name = do
    m <- ask
    case Map.lookup (Nothing, name) m of
        Nothing  -> return $ Name [] "unresolved" 0
            --error $ "Unresolved identifier: " ++ show (name, Map.keys m)
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
    m <- ask
    let name = case Map.lookup (Just (variableType var), variableName var) m of
            Nothing  -> Name [] "unresolved" 0
            --error $ "Unresolved identifier: " ++ show (name, Map.keys m)
            Just new -> new
    return $ var{ variableName = name }

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
        <*> pure attrs
        <*> mapM resolve args
        <*> pure rets
        <*> uniqBlock body

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
        Invoke fn vars ->
            Invoke <$> resolve fn <*> mapM resolve vars
        InvokeHandler cont exception ->
            InvokeHandler <$> resolve cont <*> resolve exception
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
        Alloc n ->
            pure (Alloc n)
        Store nodeName vars ->
            Store <$> resolveNodeName nodeName <*> mapM resolve vars
        BumpHeapPtr n -> pure $ BumpHeapPtr n
        Write ptr idx var ->
            Write <$> resolve ptr <*> pure idx <*> resolve var
        Address var idx ->
            Address <$> resolve var <*> pure idx
        FunctionPointer fn -> FunctionPointer <$> resolveName fn
        Fetch constant var ->
            Fetch constant <$> resolve var
        Load constant var idx ->
            Load constant <$> resolve var <*> pure idx
        Add a b ->
            Add <$> resolve a <*> resolve b
        Undefined -> pure Undefined
        Save var nth -> Save <$> resolve var <*> pure nth
        Restore nth -> pure $ Restore nth
        Eval var ->
            Eval <$> resolve var
        Apply a b ->
            Apply <$> resolve a <*> resolve b
        ReadRegister{} -> return simple
        WriteRegister reg var ->
            WriteRegister reg <$> resolve var
        ReadGlobal reg ->
            pure $ ReadGlobal reg
        WriteGlobal reg var ->
            WriteGlobal reg <$> resolve var
        TypeCast var ->
            TypeCast <$> resolve var
        Literal lit -> pure $ Literal lit
        MkNode name vars ->
            MkNode <$> resolveNodeName name <*> mapM resolve vars
        GCAllocate n ->
            pure (GCAllocate n)
        GCBegin -> pure GCBegin
        GCEnd -> pure GCEnd
        GCMark var ->
            GCMark <$> resolve var
        GCMarkNode var ->
            GCMarkNode <$> resolve var








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
    return $ fn
      { fnName = name
      , fnArguments = args
      , fnBody = body }

locallyMaybe :: (a -> LUniq a) -> Maybe a -> LUniq (Maybe a)
locallyMaybe _ Nothing = return Nothing
locallyMaybe fn (Just a) = Just <$> fn a


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
    Invoke fn args ->
      Invoke
        <$> locallyResolveVariable fn
        <*> mapM locallyResolveVariable args
    -- InvokeHandler Variable Variable
    Exit -> return block
    Panic{} -> return block

locallyAlternative :: Alternative -> LUniq Alternative
locallyAlternative (Alternative pattern branch) = limitScope $
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
    -- Catch Name [Variable] Name [Variable]
    Alloc{} -> pure expr
    Store name args ->
      Store
        <$> locallyNodeName name
        <*> mapM locallyResolveVariable args
    BumpHeapPtr{} -> pure expr
    Write dst offset src ->
      Write
        <$> locallyResolveVariable dst
        <*> pure offset
        <*> locallyResolveVariable src
    Address ptr offset ->
      Address
        <$> locallyResolveVariable ptr
        <*> pure offset
    FunctionPointer name ->
      FunctionPointer
        <$> locallyResolve name
    Fetch attrs ptr ->
      Fetch attrs <$> locallyResolveVariable ptr
    Load attrs ptr offset ->
      Load attrs <$> locallyResolveVariable ptr <*> pure offset
    Add a b -> Add <$> locallyResolveVariable a <*> locallyResolveVariable b
    Undefined -> pure expr
    Save val offset ->
      Save
        <$> locallyResolveVariable val
        <*> pure offset
    Restore{} -> pure expr
    ReadRegister{} -> pure expr
    WriteRegister reg val ->
      WriteRegister reg <$> locallyResolveVariable val
    ReadGlobal{} -> pure expr
    WriteGlobal reg val ->
      WriteGlobal reg <$> locallyResolveVariable val
    TypeCast arg -> TypeCast <$> locallyResolveVariable arg
    MkNode name args ->
      MkNode
        <$> locallyNodeName name
        <*> mapM locallyResolveVariable args
    Literal{} -> pure expr
    Eval arg -> Eval <$> locallyResolveVariable arg
    Apply fn a ->
      Apply
        <$> locallyResolveVariable fn
        <*> locallyResolveVariable a
    GCAllocate{} -> pure expr
    GCBegin -> pure expr
    GCEnd -> pure expr
    GCMark arg -> GCMark <$> locallyResolveVariable arg
    GCMarkNode arg -> GCMarkNode <$> locallyResolveVariable arg

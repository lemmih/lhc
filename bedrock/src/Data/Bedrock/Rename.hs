{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Bedrock.Rename (unique) where

import           Control.Applicative  (Applicative, pure, (<$>), (<*>))
import           Control.Monad.Reader
import           Control.Monad.State
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
    let (idNum, ns') = case mbTy of
            Nothing -> newGlobalID ns
            Just ty -> newIDByType ns ty
    put $ ns'
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
renameAll [] action = action
renameAll (x:xs) action = rename Nothing x (renameAll xs action)

renameVariables :: [Variable] -> Uniq a -> Uniq a
renameVariables [] action     = action
renameVariables (v:vs) action =
    rename (Just (variableType v)) (variableName v) $
    renameVariables vs action

resolveName :: Name -> Uniq Name
resolveName name = do
    m <- ask
    case Map.lookup name m of
        Nothing  -> error $ "Unresolved identifier: " ++ nameIdentifier name
        Just new -> return new

resolveNodeName :: NodeName -> Uniq NodeName
resolveNodeName nodeName =
    case nodeName of
        ConstructorName name ->
            ConstructorName <$> resolveName name
        FunctionName name blanks ->
            FunctionName <$> resolveName name <*> pure blanks

resolve :: Variable -> Uniq Variable
resolve var = do
    name <- resolveName (variableName var)
    return var{ variableName = name }

resolveArgument :: Argument -> Uniq Argument
resolveArgument arg =
    case arg of
        RefArg var            ->
            RefArg <$> resolve var
        LitArg lit            ->
            pure (LitArg lit)
        NodeArg nodeName vars ->
            NodeArg <$> resolveNodeName nodeName <*> mapM resolve vars

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
uniqFunction (Function name args rets body) = renameVariables args $
    Function
        <$> resolveName name
        <*> mapM resolve args
        <*> pure rets
        <*> uniqExpression body

uniqExpression :: Expression -> Uniq Expression
uniqExpression expr =
    case expr of
        Case scrut mbBranch alts ->
            Case
                <$> resolve scrut
                <*> uniqMaybe uniqExpression mbBranch
                <*> mapM uniqAlternative alts
        Bind binds simple rest -> renameVariables binds $
            Bind
                <$> mapM resolve binds
                <*> uniqSimple simple
                <*> uniqExpression rest
        Return vars ->
            Return <$> mapM resolve vars
        Throw var ->
            Throw <$> resolve var
        TailCall fn vars ->
            TailCall <$> resolveName fn <*> mapM resolve vars
        Invoke fn vars ->
            Invoke <$> resolve fn <*> mapM resolve vars
        Exit -> pure Exit
        Panic msg -> pure (Panic msg)

uniqAlternative :: Alternative -> Uniq Alternative
uniqAlternative (Alternative pattern branch) =
    case pattern of
        LitPat{} -> Alternative pattern <$> uniqExpression branch
        NodePat nodeName vars ->
            renameVariables vars $ do
                Alternative
                    <$> (NodePat
                        <$> resolveNodeName nodeName
                        <*> mapM resolve vars)
                    <*> uniqExpression branch


uniqMaybe :: (a -> Uniq a) -> Maybe a -> Uniq (Maybe a)
uniqMaybe fn obj =
    case obj of
        Nothing  -> return Nothing
        Just val -> Just <$> fn val

uniqSimple :: SimpleExpression -> Uniq SimpleExpression
uniqSimple simple =
    case simple of
        Literal lit ->
            pure (Literal lit)
        Application fn vars ->
            Application <$> resolveName fn <*> mapM resolve vars
        WithExceptionHandler exh exhArgs fn fnArgs ->
            WithExceptionHandler
                <$> resolveName exh <*> mapM resolve exhArgs
                <*> resolveName fn <*> mapM resolve fnArgs
        Alloc n ->
            pure (Alloc n)
        SizeOf{} -> error "uniqSimple: SizeOf"
        Store nodeName vars ->
            Store <$> resolveNodeName nodeName <*> mapM resolve vars
        Write ptr idx arg ->
            Write <$> resolve ptr <*> pure idx <*> resolveArgument arg
        Address var idx ->
            Address <$> resolve var <*> pure idx
        Fetch var ->
            Fetch <$> resolve var
        Load var idx ->
            Load <$> resolve var <*> pure idx
        Add a b ->
            Add <$> resolve a <*> resolve b
        Eval var ->
            Eval <$> resolve var
        Apply a b ->
            Apply <$> resolve a <*> resolve b
        Print var ->
            Print <$> resolve var
        ReadRegister{} -> return simple
        WriteRegister reg var ->
            WriteRegister reg <$> resolve var
        ReadGlobal{} -> error "uniqSimple: ReadGlobal"
        WriteGlobal{} -> error "uniqSimple: WriteGlobal"
        Unit args ->
            Unit <$> mapM resolveArgument args
        GCAllocate n ->
            pure (GCAllocate n)
        GCBegin -> pure GCBegin
        GCEnd -> pure GCEnd
        GCMark var ->
            GCMark <$> resolve var
        GCMarkNode var ->
            GCMarkNode <$> resolve var



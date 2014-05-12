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
    rename (Just (variableType v)) (variableName v) $
    renameVariables vs action

resolveName :: Name -> Uniq Name
resolveName name = do
    m <- ask
    case Map.lookup name m of
        Nothing  -> -- return $ Name [] "unresolved" 0
            error $ "Unresolved identifier: " ++ show (name, Map.keys m)
        Just new -> return new

resolveNodeName :: NodeName -> Uniq NodeName
resolveNodeName nodeName =
    case nodeName of
        ConstructorName name ->
            ConstructorName <$> resolveName name
        FunctionName name blanks ->
            FunctionName <$> resolveName name <*> pure blanks
        --CatchFrame name blanks ->
        --    CatchFrame <$> resolveName name <*> pure blanks

resolve :: Variable -> Uniq Variable
resolve var = do
    name <- resolveName (variableName var)
    return var{ variableName = name }

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
        Write ptr idx var ->
            Write <$> resolve ptr <*> pure idx <*> resolve var
        Address var idx ->
            Address <$> resolve var <*> pure idx
        Fetch constant var ->
            Fetch constant <$> resolve var
        Load constant var idx ->
            Load constant <$> resolve var <*> pure idx
        Add a b ->
            Add <$> resolve a <*> resolve b
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



{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Bedrock.RegisterIntroduction
    ( registerIntroduction ) where

import           Control.Applicative  (Applicative, pure, (<$>), (<*>))
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map             (Map)
import qualified Data.Map             as Map

import           Data.Bedrock
import           Data.Bedrock.Misc

type Env = Map Variable [Variable]

newtype Uniq a = Uniq { unUniq :: ReaderT Env (State AvailableNamespace) a }
    deriving ( Monad, MonadReader Env, MonadState AvailableNamespace
             , Functor, Applicative )

registerIntroduction :: Module -> Module
registerIntroduction m = evalState (runReaderT (unUniq (uniqModule m)) env) st
  where
    env = Map.empty
    st = modNamespace m



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

lower :: Variable -> Uniq a -> Uniq a
lower old action =
    case variableType old of
        StaticNode n -> do
            newNames <- replicateM n (newName (Just Node) (variableName old))
            let newVars =
                    [ Variable name (Primitive IWord) | name <- newNames ]
            local (Map.insert old newVars) action
        Node -> error $ "RegisterIntroduction: Found node: " ++ show old
        _  -> action

lowerMany :: [Variable] -> Uniq a -> Uniq a
lowerMany xs action = foldr lower action xs

resolve :: Variable -> Uniq [Variable]
resolve var = asks $ Map.findWithDefault [var] var

resolveMany :: [Variable] -> Uniq [Variable]
resolveMany = fmap concat . mapM resolve

uniqModule :: Module -> Uniq Module
uniqModule m =
    --lowerMany [ name | NodeDefinition name _args <- nodes m ] $
    do
        ns  <- mapM uniqNode (nodes m)
        fns <- mapM uniqFunction (functions m)
        namespace <- get
        return Module
            { modForeigns = modForeigns m
            , nodes = ns
            , entryPoint = entryPoint m
            , functions = fns
            , modNamespace = namespace
            }

uniqNode :: NodeDefinition -> Uniq NodeDefinition
uniqNode = return {-
uniqNode (NodeDefinition name args) = 
    NodeDefinition <$> resolveName name <*> pure args
-}
uniqFunction :: Function -> Uniq Function
uniqFunction (Function name args rets body) = lowerMany args $
    Function
        <$> pure name -- resolveName name
        <*> resolveMany args
        <*> pure rets
        <*> uniqBlock body

bindMany :: [(Variable, Variable)] -> Block -> Block
bindMany lst rest =
    foldr (\(bind, var) -> Bind [bind] (TypeCast var)) rest lst

uniqBlock :: Block -> Uniq Block
uniqBlock block =
    case block of
        Case scrut mbBranch alts -> do
            (tag:args) <- resolve scrut
            Case
                <$> pure tag
                <*> uniqMaybe uniqBlock mbBranch
                <*> mapM (uniqAlternative args) alts
        Bind [bind] (TypeCast var) rest -> lower bind $ do
            nodeBinds <- resolve bind
            varArgs   <- resolve var
            bindMany (zip nodeBinds varArgs)
                <$> uniqBlock rest
        Bind [bind] (Literal lit) rest -> lower bind $
            Bind
                <$> resolve bind
                <*> pure (Literal lit)
                <*> uniqBlock rest
        Bind [bind] (MkNode nodeName nodeArgs) rest -> lower bind $ do
            (tagBind:nodeBinds) <- resolve bind
            rest' <- uniqBlock rest
            return $
                Bind [tagBind] (MkNode nodeName []) $
                bindMany (zip nodeBinds nodeArgs) rest'
        Bind binds (Fetch ptr) rest -> lowerMany binds $ do
            binds' <- resolveMany binds
            rest' <- uniqBlock rest
            return $ foldr (\(n,b) -> Bind [b] (Load ptr n)) rest' (zip [0..] binds')
        Bind [bind] (Store nodeName args) rest -> do
            args' <- resolveMany args
            rest' <- uniqBlock rest
            return $
                Bind [] (Alloc $ 1 + length args) $
                Bind [bind] (Store nodeName args') rest'
        Bind binds simple rest -> lowerMany binds $
            Bind
                <$> resolveMany binds
                <*> uniqExpression simple
                <*> uniqBlock rest
        Return vars ->
            Return <$> resolveMany vars
        Raise var ->
            pure $ Raise var
        TailCall fn vars ->
            TailCall fn <$> resolveMany vars
        -- FIXME: the code for Invoke is wrong.
        Invoke fn vars ->
            Invoke <$> pure fn <*> resolveMany vars
        InvokeHandler{} -> error $
            "Register introduction: @InvokeHandler must have been lowered\
            \ by now"
        Exit -> pure Exit
        Panic msg -> pure (Panic msg)

uniqAlternative :: [Variable] -> Alternative -> Uniq Alternative
uniqAlternative args (Alternative pattern branch) =
    case pattern of
        LitPat{} -> Alternative pattern <$> uniqBlock branch
        NodePat nodeName vars -> lowerMany vars $ do
            flatVars <- resolveMany vars
            Alternative
                <$> pure (NodePat nodeName [])
                <*> (bindMany (zip flatVars args)
                        <$> uniqBlock branch)


uniqMaybe :: (a -> Uniq a) -> Maybe a -> Uniq (Maybe a)
uniqMaybe fn obj =
    case obj of
        Nothing  -> return Nothing
        Just val -> Just <$> fn val

uniqExpression :: Expression -> Uniq Expression
uniqExpression expr =
    case expr of
        Application fn vars ->
            Application fn <$> resolveMany vars
        CCall fn vars ->
            CCall fn <$> resolveMany vars
        Catch exh exhArgs fn fnArgs ->
            Catch
                <$> pure exh <*> resolveMany exhArgs
                <*> pure fn <*> resolveMany fnArgs
        Alloc{} -> pure expr
        Store nodeName vars ->
            Store <$> pure nodeName <*> resolveMany vars
        Fetch{} -> pure expr
        Load{} -> error "uniqSimple: Load"
        Add{} -> pure expr
        Eval{} -> pure expr
        Apply{} -> pure expr
        ReadGlobal{} -> error "uniqSimple: ReadGlobal"
        WriteGlobal{} -> error "uniqSimple: WriteGlobal"
        TypeCast{} -> pure expr
        MkNode{} -> pure expr
        Literal{} -> pure expr
        GCAllocate{} -> pure expr
        GCBegin -> pure expr
        GCEnd -> pure expr
        GCMark{} -> pure expr
        GCMarkNode{} -> pure expr
        Write{} -> pure expr
        Address{} -> pure expr
        ReadRegister{} -> pure expr
        WriteRegister{} -> pure expr



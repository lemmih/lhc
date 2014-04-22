{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Bedrock.Storage.Pluggable where

import Control.Applicative ( Applicative, (<$>) )
import Control.Monad.State
import Control.Monad.Reader

import Data.Bedrock
import Data.Bedrock.Misc


-----------------------------------------------------------
-- Pluggable GCs

data GCState = GCState
    { gcNamespace    :: AvailableNamespace
    , gcNewFunctions :: [Function]
    , gcNewForeigns  :: [Foreign] }
newtype GC a = GC { unGC :: State GCState a }
    deriving
        ( Monad, Functor, Applicative
        , MonadState GCState )

data StorageManager = StorageManager
    { smInit     :: Function
    , smBegin    :: Function
    , smEnd      :: Function
    , smMark     :: Function
    , smAllocate :: (Variable -> SimpleExpression)
    }

runGC :: Module -> GC a -> (a, Module)
runGC m action =
    case runState (unGC action) initState of
        (val, st) -> (val, mkModule st)
  where
    initState = GCState
        { gcNamespace = modNamespace m
        , gcNewFunctions = []
        , gcNewForeigns = [] }
    mkModule st = m
        { modNamespace = gcNamespace st
        , functions = functions m ++ gcNewFunctions st
        , modForeigns = modForeigns m ++ gcNewForeigns st }

-----------------------------------------------------------
-- Lower

--GCAllocate n -> call pluginAllocate(n)
--GCInit -> call pluginInit
--GCBegin -> call
--GCEnd -> call 
--GCMark -> call
--GCMarkNode -> ?

lowerGC :: Module -> StorageManager -> Module
lowerGC m sm = m{ functions = loweredFunctions ++ newFunctions }
  where
    loweredFunctions = mapM (lowerFunction (entryPoint m)) (functions m) sm
    newFunctions = map ($sm) [smInit, smBegin, smEnd, smMark]

lowerFunction :: Name -> Function -> StorageManager -> Function
lowerFunction entry fn = do
    body <- lowerExpression (fnBody fn)
    sm <- ask
    let initName = fnName (smInit sm)
    if fnName fn == entry
        then return fn{ fnBody = Bind [] (Application initName []) body }
        else return fn{ fnBody = body }

lowerExpression :: Expression -> StorageManager -> Expression
lowerExpression expr =
    case expr of
        Bind binds simple rest ->
            lowerSimple binds simple =<< lowerExpression rest
        Case scrut defaultBranch alts ->
            Case scrut defaultBranch <$> mapM lowerAlternative alts
        Return{} -> return expr
        TailCall{} -> return expr
        Exit -> return expr
        Panic{} -> return expr
        _ -> error $ "Storage.Pluggable: Unhandled: " ++ show expr

lowerAlternative :: Alternative -> StorageManager -> Alternative
lowerAlternative (Alternative pattern branch) =
    Alternative pattern <$> lowerExpression branch

lowerSimple :: [Variable] -> SimpleExpression -> Expression
            -> StorageManager -> Expression
lowerSimple binds simple rest sm =
    case simple of
        GCAllocate n ->
            Bind [size] (Unit [LitArg (LiteralInt $ fromIntegral n)]) $
            ret $ smAllocate sm size
        GCBegin ->
            ret $ Application (fnName (smBegin sm)) []
        GCEnd ->
            ret $ Application (fnName (smEnd sm)) []
        GCMark ptr ->
            ret $ Application (fnName (smMark sm)) [ptr]
        GCMarkNode{} ->
            error "Storage.Pluggable: Can't deal with GCMarkNode"
        _ -> ret simple
  where
    size = Variable (Name [] "size" 0) Primitive
    ret s = Bind binds s rest

-----------------------------------------------------------
-- 

newName :: String -> GC Name
newName identifier = do
    ns <- gets gcNamespace
    let (idNum, ns') = newGlobalID ns
    modify $ \st -> st{gcNamespace = ns'}
    return $ Name [] identifier idNum

newVariable :: String -> Type -> GC Variable
newVariable identifier ty = do
    name <- newName identifier
    return $ Variable name ty

pushFunction :: Function -> GC ()
pushFunction fn = modify $ \st ->
    st{ gcNewFunctions = fn : gcNewFunctions st }

pushForeign :: Foreign -> GC ()
pushForeign f = modify $ \st ->
    st{ gcNewForeigns = f : gcNewForeigns st }


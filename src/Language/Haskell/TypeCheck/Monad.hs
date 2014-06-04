{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.TypeCheck.Monad where

import Data.List
import Data.IORef
import           Control.Applicative
import           Control.Monad.State
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Annotated.Syntax (Type(..),Name,ann)

import           Language.Haskell.Scope
import           Language.Haskell.TypeCheck.Types

data TcEnv = TcEnv
    { -- Globals such as Nothing, Just, etc
      tcEnvGlobals   :: Map GlobalName TcType
    , tcEnvVariables :: Map SrcSpanInfo TcType
    , tcEnvUnique    :: Int
    }
newtype TI a = TI { unTI :: StateT TcEnv IO a }
    deriving ( Monad, Functor, Applicative, MonadState TcEnv, MonadIO )

--type Infer a = a Origin -> TI (a Typed)

runTI :: TI a -> IO TcEnv
runTI action = execStateT (unTI f) env
  where
    f = do
        action
        vars <- gets tcEnvVariables
        vars' <- forM (Map.assocs vars) $ \(src, ty) -> do
            ty' <- zonk ty
            return (src, ty')
        modify $ \st -> st{tcEnvVariables = Map.fromList vars'}
    env = TcEnv
        { tcEnvGlobals   = Map.empty
        , tcEnvVariables = Map.empty
        , tcEnvUnique    = 0 }

newUnique :: TI Int
newUnique = do
    u <- gets tcEnvUnique
    modify $ \env -> env{ tcEnvUnique = u + 1 }
    return u

getFreeMetaVariables :: TI [TcMetaVar]
getFreeMetaVariables = do
    m <- gets tcEnvVariables
    return $ nub $ concatMap metaVariables (Map.elems m)

setAssumption :: SrcSpanInfo -> TcType -> TI ()
setAssumption ident tySig = modify $ \env ->
    env{ tcEnvVariables = Map.insert ident tySig (tcEnvVariables env) }

findAssumption :: SrcSpanInfo -> TI TcType
findAssumption ident = do
    m <- gets tcEnvVariables
    case Map.lookup ident m of
        Nothing -> error $ "Missing ident: " ++ show ident
        Just scheme -> return scheme

setGlobal :: GlobalName -> TcType -> TI ()
setGlobal gname scheme = modify $ \env ->
    env{ tcEnvGlobals = Map.insert gname scheme (tcEnvGlobals env) }

findGlobal :: GlobalName -> TI TcType
findGlobal gname = do
    m <- gets tcEnvGlobals
    case Map.lookup gname m of
        Nothing -> error $ "Missing global: " ++ show gname
        Just scheme -> return scheme

freshInst :: TcType -> TI (Qual TcType)
freshInst (TcForall tyvars (preds :=> t0)) = do
    refs <- replicateM (length tyvars) newTcVar
    let subst = zip tyvars refs
        instPred (IsIn className ty) =
            IsIn className (instantiate ty)
        instantiate ty =
            case ty of
                TcForall{} -> error "freshInst"
                TcFun a b -> TcFun (instantiate a) (instantiate b)
                TcApp a b -> TcApp (instantiate a) (instantiate b)
                TcRef v ->
                    case lookup v subst of
                        Nothing -> TcRef v
                        Just ref -> TcMetaVar ref
                TcCon{} -> ty
                TcMetaVar{} -> ty -- FIXME: Is this an error?
    return $ map instPred preds :=> instantiate t0
freshInst ty = pure ([] :=> ty)

unify :: TcType -> TcType -> TI ()
unify (TcApp la lb) (TcApp ra rb) = do
    unify la ra
    unify lb rb
unify (TcFun la lb) (TcFun ra rb) = do
    unify la ra
    unify lb rb
unify (TcCon left) (TcCon right) =
    if left == right
        then return ()
        else error $ "unify con: " ++ show (left,right)
unify (TcMetaVar ref) a = unifyMetaVar ref a
unify a (TcMetaVar ref) = unifyMetaVar ref a
unify a b               = error $ "unify: " ++ show (a,b)

unifyMetaVar :: TcMetaVar -> TcType -> TI ()
unifyMetaVar a (TcMetaVar b) | a == b = return ()
unifyMetaVar (TcMetaRef _ident ref) rightTy = do
    mbSubst <- liftIO $ readIORef ref
    case mbSubst of
        Just leftTy -> unify leftTy rightTy
        Nothing -> liftIO $ writeIORef ref (Just rightTy)

zonk :: TcType -> TI TcType
zonk ty =
    case ty of
        TcForall{} -> pure ty
        TcFun a b -> TcFun <$> zonk a <*> zonk b
        TcApp a b -> TcApp <$> zonk a <*> zonk b
        TcRef{}   -> pure ty
        TcCon{}   -> pure ty
        TcMetaVar (TcMetaRef _name meta) -> do
            mbTy <- liftIO (readIORef meta)
            case mbTy of
                Nothing -> pure ty
                Just sub -> zonk sub

tcVarFromName :: Name Origin -> TcVar
tcVarFromName name =
    TcVar (getNameIdentifier name) src
  where
    Origin (Resolved (GlobalName src _qname)) _ = ann name

newTcVar :: TI TcMetaVar
newTcVar = do
    u <- newUnique
    ref <- liftIO $ newIORef Nothing
    return $ TcMetaRef ("v"++show u) ref

typeToTcType :: Type Origin -> TcType
typeToTcType ty =
    case ty of
        TyFun _ a b -> TcFun (typeToTcType a) (typeToTcType b)
        TyVar _ name -> TcRef (tcVarFromName name)
        TyCon _ qname ->
            let Origin (Resolved gname) _ = ann qname
            in TcCon gname
        TyApp _ a b -> TcApp (typeToTcType a) (typeToTcType b)
        TyParen _ t -> typeToTcType t
        _ -> error $ "typeToTcType: " ++ show ty

--tcTypeToScheme :: TcType -> TcType
--tcTypeToScheme ty = Scheme (freeTcVariables ty) ([] :=> ty)

explicitTcForall :: TcType -> TcType
explicitTcForall ty = TcForall (freeTcVariables ty) ([] :=> ty)

freeTcVariables :: TcType -> [TcVar]
freeTcVariables = worker []
  where
    worker ignore ty =
        case ty of
            TcForall{} -> error "freeTcVariables"
            TcFun a b -> worker ignore a ++ worker ignore b
            TcApp a b -> worker ignore a ++ worker ignore b
            TcRef v | v `elem` ignore -> []
                    | otherwise       -> [v]
            TcCon{} -> []
            TcMetaVar{} -> []

metaVariables :: TcType -> [TcMetaVar]
metaVariables ty =
    case ty of
        -- XXX: There shouldn't be any meta variables inside a forall scope.
        TcForall _ (_ :=> ty') -> metaVariables ty'
        TcFun a b -> metaVariables a ++ metaVariables b
        TcApp a b -> metaVariables a ++ metaVariables b
        TcRef{} -> []
        TcCon{} -> []
        TcMetaVar var -> [var]

-- Replace free meta vars with tcvars. Compute the smallest context.
-- 
generalize :: [TcMetaVar] -> TcType -> TI TcType
generalize free ty = do
    forM_ unbound $ \var@(TcMetaRef _name ref) ->
        liftIO $ writeIORef ref (Just (TcRef (toTcVar var)))
    ty' <- zonk ty
    return $ TcForall (map toTcVar unbound) ([] :=> ty')
  where
    unbound = nub (metaVariables ty) \\ free
    toTcVar (TcMetaRef name _) = TcVar name noSrcSpanInfo
    --replace ty =
    --    case ty of
    --        TcForall{} -> error "generalize"
    --        TcFun a b -> TcFun (replace a) (replace b)
    --        TcApp a b -> TcApp (replace a) (replace b)
    --        TcRef{}   -> ty
    --        TcCon{}   -> ty
    --        TcMetaVar var
    --            | var `elem` unbound -> TcRef (toTcVar var)
    --            | otherwise          -> ty

noSrcSpanInfo :: SrcSpanInfo
noSrcSpanInfo = infoSpan (mkSrcSpan noLoc noLoc) []

mkBuiltIn :: String -> String -> GlobalName
mkBuiltIn m ident = GlobalName noSrcSpanInfo (QualifiedName m ident)

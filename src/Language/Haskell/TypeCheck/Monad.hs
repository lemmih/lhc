{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.TypeCheck.Monad where

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
      tcEnvGlobals   :: Map GlobalName Scheme
    , tcEnvVariables :: Map SrcLoc Scheme
    , tcEnvUnique    :: Int
    }
newtype TI a = TI { unTI :: StateT TcEnv IO a }
    deriving ( Monad, Functor, Applicative, MonadState TcEnv, MonadIO )

type Infer a = a Scoped -> TI (a Typed)

runTI :: TI a -> IO TcEnv
runTI action = execStateT (unTI action) env
  where
    env = TcEnv
        { tcEnvGlobals   = Map.empty
        , tcEnvVariables = Map.empty
        , tcEnvUnique    = 0 }

newUnique :: TI Int
newUnique = do
    u <- gets tcEnvUnique
    modify $ \env -> env{ tcEnvUnique = u + 1 }
    return u

setAssumption :: SrcLoc -> Scheme -> TI ()
setAssumption ident tySig = modify $ \env ->
    env{ tcEnvVariables = Map.insert ident tySig (tcEnvVariables env) }

findAssumption :: SrcLoc -> TI Scheme
findAssumption ident = do
    m <- gets tcEnvVariables
    case Map.lookup ident m of
        Nothing -> error $ "Missing ident: " ++ show ident
        Just scheme -> return scheme

setGlobal :: GlobalName -> Scheme -> TI ()
setGlobal gname scheme = modify $ \env ->
    env{ tcEnvGlobals = Map.insert gname scheme (tcEnvGlobals env) }

findGlobal :: GlobalName -> TI Scheme
findGlobal gname = do
    m <- gets tcEnvGlobals
    case Map.lookup gname m of
        Nothing -> error $ "Missing global: " ++ show gname
        Just scheme -> return scheme

-- FIXME: Qualified TcType
freshInst :: Scheme -> TI (Qual TcType)
freshInst (Scheme tyvars (preds :=> t0)) = do
    refs <- replicateM (length tyvars) newTcVar
    let subst = zip tyvars refs
        instPred (IsIn className ty) =
            IsIn className (instantiate ty)
        instantiate ty =
            case ty of
                TcForall -> error "freshInst"
                TcFun a b -> TcFun (instantiate a) (instantiate b)
                TcApp a b -> TcApp (instantiate a) (instantiate b)
                TcVar v ->
                    case lookup v subst of
                        Nothing -> TcVar v
                        Just ref -> TcMetaVar ref
                TcCon{} -> ty
                TcMetaVar{} -> ty -- FIXME: Is this an error?
    return $ map instPred preds :=> instantiate t0

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
        else error "unify con"
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
        TcForall -> return TcForall
        TcFun a b -> TcFun <$> zonk a <*> zonk b
        TcApp a b -> TcApp <$> zonk a <*> zonk b
        TcVar{}   -> pure ty
        TcCon{}   -> pure ty
        TcMetaVar (TcMetaRef _name meta) -> do
            mbTy <- liftIO (readIORef meta)
            case mbTy of
                Nothing -> pure ty
                Just sub -> zonk sub

tcVarFromName :: Name Scoped -> TcVar
tcVarFromName name =
    (getNameIdentifier name, src)
  where
    Scoped (TypeVar src) _ = ann name

newTcVar :: TI TcMetaVar
newTcVar = do
    u <- newUnique
    ref <- liftIO $ newIORef Nothing
    return $ TcMetaRef ("v"++show u) ref

typeToTcType :: Type Scoped -> TcType
typeToTcType ty =
    case ty of
        TyFun _ a b -> TcFun (typeToTcType a) (typeToTcType b)
        TyVar _ name -> TcVar (tcVarFromName name)
        TyCon _ qname ->
            let Scoped (Global gname) _ = ann qname
            in TcCon gname
        _ -> error $ "typeToTcType: " ++ show ty

tcTypeToScheme :: TcType -> Scheme
tcTypeToScheme ty = Scheme (freeTcVariables ty) ([] :=> ty)

freeTcVariables :: TcType -> [TcVar]
freeTcVariables = worker []
  where
    worker ignore ty =
        case ty of
            TcForall -> error "freeTcVariables"
            TcFun a b -> worker ignore a ++ worker ignore b
            TcApp a b -> worker ignore a ++ worker ignore b
            TcVar v | v `elem` ignore -> []
                    | otherwise       -> [v]
            TcCon{} -> []
            TcMetaVar{} -> []



{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler.HaskellToCore
    ( convert
    ) where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Maybe
import qualified Language.Haskell.Exts.Annotated  as HS

import           Compiler.Core
import           Data.Bedrock                     (AvailableNamespace (..),
                                                   CType (..), Foreign (..),
                                                   Name (..),
                                                   Type (..))
import           Data.Bedrock.Misc
import           Language.Haskell.Scope           (GlobalName (..),
                                                   NameInfo (..), Origin (..),
                                                   QualifiedName (..),
                                                   getNameIdentifier)
import qualified Language.Haskell.Scope           as Scope
import           Language.Haskell.TypeCheck.Monad (TcEnv (..), mkBuiltIn)
import           Language.Haskell.TypeCheck.Types (Coercion (..),TcType(..),Qual(..))

import           Debug.Trace

data Scope = Scope
    { scopeVariables    :: Map GlobalName Name
    , scopeNodes        :: Map QualifiedName Name
    , scopeConstructors :: Map GlobalName Name -- XXX: Merge with scopeNodes?
    , scopeTcEnv        :: TcEnv
    , scopeArity        :: Map GlobalName Int
    }
instance Monoid Scope where
    mempty = Scope
        { scopeVariables    = Map.empty
        , scopeNodes        = Map.empty
        , scopeConstructors = Map.empty
        , scopeTcEnv        = TcEnv
            { -- Globals such as Nothing, Just, etc
              tcEnvValues    = Map.empty
            , tcEnvUnique    = 0
            , tcEnvCoercions = Map.empty
            }
        , scopeArity         = Map.empty
        }
    mappend a b = Scope
        { scopeVariables    = w scopeVariables
        , scopeNodes        = w scopeNodes
        , scopeConstructors = w scopeConstructors
        , scopeTcEnv        = scopeTcEnv a
        , scopeArity        = w scopeArity }
        where w f = mappend (f a) (f b)

data Env = Env
    { envScope        :: Scope
    , envForeigns     :: [Foreign]
    , envNodes        :: [NodeDefinition]
    , envNewTypes     :: [NewType]
    , envDecls        :: [Decl]
    , envConstructors :: Map Name Name
    }

instance Monoid Env where
    mempty = Env
        { envScope    = mempty
        , envForeigns = mempty
        , envNodes    = mempty
        , envNewTypes = mempty
        , envDecls    = mempty
        , envConstructors = mempty
        }
    mappend a b = Env
        { envScope    = w envScope
        , envForeigns = w envForeigns
        , envNodes    = w envNodes
        , envNewTypes = w envNewTypes
        , envDecls    = w envDecls
        , envConstructors = w envConstructors
        }
        where w f = mappend (f a) (f b)

newtype M a = M { unM :: RWS Scope Env AvailableNamespace a }
    deriving
        ( Monad, Functor, Applicative
        , MonadReader Scope, MonadState AvailableNamespace
        , MonadWriter Env )

runM :: TcEnv -> M a -> (AvailableNamespace, Env)
runM tcEnv m = (ns', env)
  where
    (ns', env) = execRWS (unM m) ((envScope env){ scopeTcEnv = tcEnv }) ns
    ns = AvailableNamespace 0 0 0 0

pushForeign :: Foreign -> M ()
pushForeign f = tell mempty{ envForeigns = [f] }

pushDecl :: Decl -> M ()
pushDecl decl = tell mempty{ envDecls = [decl] }

pushNode :: NodeDefinition -> M ()
pushNode def = tell mempty{ envNodes = [def] }

pushNewType :: NewType -> M ()
pushNewType def = tell mempty{ envNewTypes = [def] }

newUnique :: M Int
newUnique = do
    ns <- get
    let (idNum, ns') = newGlobalID ns
    put ns'
    return idNum

newName :: String -> M Name
newName ident = do
    u <- newUnique
    return $ Name [] ident u

bindName :: HS.Name Origin -> M Name
bindName hsName =
    case info of
        Scope.Resolved gname@(GlobalName src qname@(QualifiedName m ident)) -> do
            let name = Name [m] (getNameIdentifier hsName) 0
            tell $ mempty{envScope = mempty
                { scopeVariables = Map.singleton gname name } }
            return name
        _ -> error "bindName"
  where
    Origin info _ = HS.ann hsName

bindVariable :: HS.Name Origin -> M Variable
bindVariable hsName = do
    name <- bindName hsName
    ty <- lookupType hsName
    return $ Variable name ty
  where
    Origin info _ = HS.ann hsName

lookupType :: HS.Name Origin -> M TcType
lookupType hsName = do
    case info of
        Resolved gname -> do
            tcEnv <- asks scopeTcEnv
            case Map.lookup gname (tcEnvValues tcEnv) of
                Nothing -> error "Missing type info"
                Just ty -> return ty
  where
    Origin info _ = HS.ann hsName

bindConstructor :: HS.Name Origin -> Int -> M Name
bindConstructor dataCon arity =
    case info of
        Resolved global@(GlobalName src qname@(QualifiedName m ident)) -> do
            let n = Name [m] ident 0
            tell $ mempty{envScope = mempty
                { scopeNodes = Map.singleton qname n
                , scopeVariables = Map.singleton global n
                , scopeArity = Map.singleton global arity } }
            return n
        _ -> error "bindName"
  where
    Origin info _ = HS.ann dataCon

resolveName :: HS.Name Origin -> M Name
resolveName hsName =
    case info of
        Scope.Resolved gname@(GlobalName src qname@(QualifiedName m ident)) -> do
            let name = Name [m] (getNameIdentifier hsName) 0
            return name
        -- Resolved gname -> do
        --     asks $ Map.findWithDefault scopeError gname . scopeVariables
        --Scope.Global gname ->
        --    asks $ Map.findWithDefault scopeError gname . scopeConstructors
        _ -> error "resolveName"
  where
    Origin info _ = HS.ann hsName
    scopeError = error $ "resolveName: Not in scope: " ++
                    getNameIdentifier hsName

resolveQualifiedName :: QualifiedName -> M Name
resolveQualifiedName qname =
    asks $ Map.findWithDefault scopeError qname . scopeNodes
  where
    scopeError = error $ "resolveGlobalName: Not in scope: " ++ show qname

resolveQName :: HS.QName Origin -> M Name
resolveQName qname =
    case qname of
        HS.Qual _ _ name -> resolveName name
        HS.UnQual _ name -> resolveName name
        _ -> error "HaskellToCore.resolveQName"

unQName :: HS.QName Origin -> HS.Name Origin
unQName qname =
    case qname of
        HS.Qual _ _ name -> name
        HS.UnQual _ name -> name

-- XXX: Ugly, ugly code.
resolveQGlobalName :: HS.QName Origin -> M Name
resolveQGlobalName qname =
    case qname of
        HS.Qual _ _ name -> worker name
        HS.UnQual _ name -> worker name
        _ -> error "HaskellToCore.resolveQName"
  where
    worker name =
        let Origin (Resolved (GlobalName _ qname)) _ = HS.ann name
        in resolveQualifiedName qname

findCoercion :: HS.SrcSpanInfo -> M Coercion
findCoercion src = do
    tiEnv <- asks scopeTcEnv
    return $ Map.findWithDefault CoerceId src (tcEnvCoercions tiEnv)


--resolveConstructor :: HS.QName Scoped -> M Name
--resolveConstructor con = do
--    name <- resolveQName con
--    asks

convert :: TcEnv -> HS.Module Origin -> Module
convert tcEnv (HS.Module _ _ _ _ decls) = Module
    { coreForeigns  = envForeigns env
    , coreDecls     = envDecls env
    , coreNodes     = envNodes env
    , coreNewTypes  = envNewTypes env
    , coreNamespace = ns }
  where
    (ns, env) = runM tcEnv $ do
        mapM_ convertDecl decls
convert _ _ = error "HaskellToCore.convert"

convertDecl :: HS.Decl Origin -> M ()
convertDecl decl =
    case decl of
        HS.FunBind _ [HS.Match _ name pats rhs _] -> do
            let Origin _ src = HS.ann name
            coercion <- findCoercion src
            pushDecl =<< Decl
                <$> lookupType name
                <*> bindName name
                <*> (WithCoercion coercion <$> convertPats pats rhs)
            -- (convertName name, convertPats pats rhs)
        HS.PatBind _ (HS.PVar _ name) rhs _binds ->
            pushDecl =<< Decl
                <$> lookupType name
                <*> bindName name
                <*> convertRhs rhs
        HS.ForImp _ _conv _safety mbExternal name ty -> do
            let external = fromMaybe (getNameIdentifier name) mbExternal
            foreignTy <- lookupType name
            pushDecl =<< Decl
                <$> lookupType name
                <*> bindName name
                <*> convertExternal external foreignTy

            unless (isPrimitive external) $ do
                let (argTypes, _isIO, retType) = ffiTypes foreignTy
                pushForeign $ Foreign
                    { foreignName = external
                    , foreignReturn = toCType retType
                    , foreignArguments = map toCType argTypes }

        HS.DataDecl _ HS.DataType{} _ctx _dhead qualCons _deriving ->
            mapM_ (convertQualCon False) qualCons
        HS.DataDecl _ HS.NewType{} _ctx _dhead qualCons _deriving ->
            mapM_ (convertQualCon True) qualCons
        _ -> return ()

isPrimitive "realWorld" = True
isPrimitive _ = False

-- XXX: Don't use Bool for isNewtype
convertQualCon :: Bool -> HS.QualConDecl Origin -> M ()
convertQualCon isNewtype (HS.QualConDecl _ _tyvars _ctx con) =
    convertConDecl isNewtype con

-- XXX: Don't use Bool for isNewtype
convertConDecl :: Bool -> HS.ConDecl Origin -> M ()
convertConDecl isNewtype con =
    case con of
        HS.ConDecl _ name tys -> do

            u <- newUnique
            let mkCon = Name [] ("mk" ++ getNameIdentifier name) u

            conName <- bindConstructor name (length tys)

            argNames <- replicateM (length tys) (newName "arg")
            ty <- lookupType name
            let args = zipWith Variable argNames (splitTy ty)
            -- pushDecl $ Decl ty mkCon (Lam args $ Con conName args)

            -- pushNode $ NodeDefinition conName (init $ splitTy ty)
            if isNewtype
                then pushNewType $ NewType conName
                else pushNode $ NodeDefinition conName (init $ splitTy ty)
        --HS.RecDecl _ name fieldDecls -> do
        _ -> error "convertCon"
  where
    -- XXX: Temporary measure. 2014-07-11
    splitTy (TcForall _ (_ :=> ty)) = splitTy ty
    splitTy (TcFun a b) = a : splitTy b
    splitTy ty = [ty]

toCType :: TcType -> CType
toCType ty =
    case ty of
        TcApp (TcCon qname) ty'
            | qname == mkBuiltIn "LHC.Prim" "Addr" ->
                CPointer (toCType ty')
        TcCon qname
            | qname == mkBuiltIn "LHC.Prim" "I8" ->
                I8
            | qname == mkBuiltIn "LHC.Prim" "I32" ->
                I32
            | qname == mkBuiltIn "LHC.Prim" "Int32" ->
                I32
            | qname == mkBuiltIn "LHC.Prim" "I64" ->
                I64
            | qname == mkBuiltIn "LHC.Prim" "Unit" ->
                CVoid
        TcApp (TcCon qname) ty'
            | qname == mkBuiltIn "LHC.Prim" "IO" ->
                toCType ty'
        TcCon qname
            | qname == mkBuiltIn "LHC.Prim" "RealWorld#" ->
                I64
        _ -> error $ "toCType: " ++ show ty

-- convertBangType :: HS.BangType Origin -> M Type
-- convertBangType bty =
--     case bty of
--         HS.UnBangedTy _ ty -> convertType ty
--         HS.BangedTy _ ty -> convertType ty
--         _ -> error "convertBangType"

convertType :: HS.Type Origin -> M Type
convertType ty =
    case ty of
        --HS.TyCon _ qname
        --    | toGlobalName qname == GlobalName "Main" "I8"
        --    -> pure $ Primitive I8
        --    | toGlobalName qname == GlobalName "Main" "I32"
        --    -> pure $ Primitive I32
        --    | toGlobalName qname == GlobalName "Main" "I64"
        --    -> pure $ Primitive I64
        --    | toGlobalName qname == GlobalName "Main" "RealWorld"
        --    -> pure NodePtr -- $ Primitive CVoid
        --HS.TyApp _ (HS.TyCon _ qname) sub
        --    | toGlobalName qname == GlobalName "Main" "Addr"
        --    -> do
        --        subTy <- convertType sub
        --        case subTy of
        --            Primitive p -> pure $ Primitive (CPointer p)
        --            _ -> error "Addr to non-primitive type"
        HS.TyParen _ sub ->
            convertType sub
        HS.TyVar{} -> pure NodePtr
        HS.TyCon{} -> pure NodePtr
        HS.TyFun{} -> pure NodePtr
        _ -> error $ "convertType: " ++ show ty -- pure NodePtr


-- cfun :: Addr I8 -> IO ()
-- \ptr -> IO (\s -> WithExternal cfun Void [ptr,s]) (IOUnit boxed s))
-- cfun :: Addr I8 -> IO CInt
-- \ptr -> IO (\s -> WithExternal cfun CInt [ptr,s]) (IOUnit boxed s))
-- cfun :: CInt -> CInt
-- \cint -> WithExternal cfun [cint] boxed
convertExternal :: String -> TcType -> M Expr
convertExternal "realworld#" _ty = return (Lit (LitInt 0))
convertExternal cName ty
    | isIO = do
        args <- forM argTypes $ \t -> Variable <$> newName "arg" <*> pure t
        primArgs <- return args
        primOut <- Variable <$> newName "primOut" <*> pure i32
        s <- Variable
                <$> newName "s"
                <*> pure (TcCon $ mkBuiltIn "LHC.Prim" "RealWorld#")
        boxed <- Variable <$> newName "boxed" <*> pure retType

        io <- resolveQualifiedName $ mkBuiltIn "LHC.Prim" "IO"
        -- ioUnit <- resolveQualifiedName $ mkBuiltIn "LHC.Prim" "IOUnit"
        cint <- resolveQualifiedName $ mkBuiltIn "LHC.Prim" "Int32"

        return $
            Lam args $
            let action = Lam [s] $
                    WithExternal primOut cName primArgs s $
                    Let (NonRec boxed $ App (Con cint) (Var primOut)) $
                    UnboxedTuple [s, boxed]
            in (App (Con io) action)
    | otherwise = do -- not isIO
        args <- forM argTypes $ \t -> Variable <$> newName "arg" <*> pure t
        primOut <- Variable <$> newName "primOut" <*> pure retType
        return $
            Lam args $
            ExternalPure primOut cName args $
            Var primOut
  where
    (argTypes, isIO, retType) = ffiTypes ty
    i32 = TcCon $ mkBuiltIn "LHC.Prim" "I32"
-- convertExternal cName ty
--     | isIO      = do
--         out <- newName "out"
--         boxed <- newName "boxed"
--         let outV = Variable out retType
--             boxedV = Variable boxed retType
--         io <- resolveQualifiedName $ mkBuiltIn "LHC.Prim" "IO"
--         unit <- resolveQualifiedName $ mkBuiltIn "LHC.Prim" "IOUnit"
--         cint <- resolveQualifiedName $ mkBuiltIn "LHC.Prim" "Int32"
--         pure $ Lam args $ App (Lam [tmp] (App (Con io) (Var tmp)))
--                 (Lam [s]
--             (WithExternal outV cName args s
--                 (Let (NonRec boxedV $ App (Con cint) (Var outV)) $
--                     App (App (Con unit) (Var boxedV)) (Var s))))
--     -- | otherwise = pure $ Lam args (ExternalPure cName retType args)
--   where
--     tmp = Variable (Name [] "tmp" 0) TcUndefined
--     s = Variable (Name [] "s" 0) TcUndefined -- NodePtr
--     (argTypes, isIO, retType) = ffiTypes ty
--     args =
--         [ Variable (Name [] "arg" 0) t -- (Primitive t)
--         | t <- argTypes ]

--packCType :: CType -> Expr -> M Expr
--packCType

ffiTypes :: TcType -> ([TcType], Bool, TcType)
ffiTypes = worker []
  where
    worker acc ty =
        case ty of
            TcFun t ty' -> worker (t : acc) ty'
            TcApp (TcCon qname) sub
                | qname == mkBuiltIn "LHC.Prim" "IO"
                    -> (reverse acc, True, sub)
            _ -> (reverse acc, False, ty)
            --_ -> error "ffiArguments"

convertPats :: [HS.Pat Origin] -> HS.Rhs Origin -> M Expr
convertPats [] rhs = convertRhs rhs
convertPats pats rhs =
    Lam <$> sequence [ bindVariable name
                    | HS.PVar _ name <- pats ]
        <*> (convertRhs rhs)

convertRhs :: HS.Rhs Origin -> M Expr
convertRhs rhs =
    case rhs of
        HS.UnGuardedRhs _ expr -> convertExp expr
        _ -> error "convertRhs"

convertExp :: HS.Exp Origin -> M Expr
convertExp expr =
    case expr of
        HS.Var _ name -> do
            let Origin _ src = HS.ann name
            coercion <- findCoercion src
            n <- resolveQName name
            ty <- lookupType $ unQName name
            return $ WithCoercion coercion (Var (Variable n ty))
        HS.Con _ name -> do
            n <- resolveQName name
            return $ Con n
        HS.App _ a b ->
            App
                <$> convertExp a
                <*> convertExp b
        HS.InfixApp _ a (HS.QVarOp _ var) b -> do
            ae <- convertExp a
            be <- convertExp b
            n <- resolveQName var
            ty <- lookupType $ unQName var
            pure $ App (App (Var (Variable n ty)) ae) be
        HS.Paren _ sub -> convertExp sub
        HS.Lambda _ pats sub ->
            Lam
                <$> sequence [ bindVariable name
                        | HS.PVar _ name <- pats ]
                <*> convertExp sub
        HS.Case _ scrut alts -> do
            scrut' <- convertExp scrut
            scrutVar <- Variable <$> newName "scrut" <*> exprType scrut'
            def <- convertAlts scrutVar alts
            return $ Case scrut' scrutVar (Just def) []
            -- Case
            --     <$> convertExp scrut
            --     <*> pure Nothing
            --     <*> mapM convertAlt alts
        HS.Lit _ lit -> pure $ Lit (convertLiteral lit)
        HS.Tuple  _ HS.Unboxed exprs -> do
            vars <- forM exprs $ \(HS.Var _ name) -> do
                n <- resolveQName name
                ty <- lookupType $ unQName name
                return $ Variable n ty
            return $ UnboxedTuple vars
        _ -> error $ "convertExp: " ++ show expr

convertAlts :: Variable -> [HS.Alt Origin] -> M Expr
convertAlts scrut [] = pure $ Case (Var scrut) scrut Nothing []
convertAlts scrut [HS.Alt _ pat rhs Nothing] =
    convertAltPat scrut Nothing pat =<< convertRhs rhs
convertAlts scrut (HS.Alt _ pat rhs Nothing:alts) = do
    rest <- convertAlts scrut alts
    restBranch <- Variable <$> newName "branch" <*> exprType rest
    if isSimplePat pat
        then
            convertAltPat scrut (Just rest) pat =<< convertRhs rhs
        else do
            e <- convertAltPat scrut (Just $ Var restBranch) pat =<< convertRhs rhs
            return $ Let (NonRec restBranch rest) e

isSimplePat :: HS.Pat Origin -> Bool
isSimplePat pat =
    case pat of
        HS.PApp _ name pats -> all isPVar pats
        HS.PVar{} -> True
        HS.PLit{} -> True
        _ -> False
  where
    isPVar HS.PVar{} = True
    isPVar _ = False

convertAltPat :: Variable -> Maybe Expr -> HS.Pat Origin -> Expr -> M Expr
convertAltPat scrut failBranch pat successBranch =
    case pat of
        HS.PApp _ name pats -> do
            args <- sequence [ Variable <$> bindName var <*> lookupType var
                             | HS.PVar _ var <- pats ]
            alt <- Alt <$> (ConPat <$> resolveQGlobalName name <*> pure args)
                <*> pure successBranch
            return $ Case (Var scrut) scrut failBranch [alt]
        HS.PTuple _ HS.Unboxed pats -> do
            args <- sequence [ Variable <$> bindName var <*> lookupType var
                             | HS.PVar _ var <- pats ]
            alt <- Alt (UnboxedPat args)
                <$> pure successBranch
            return $ Case (Var scrut) scrut failBranch [alt]
        HS.PWildCard _ ->
            return successBranch
        HS.PVar _ var -> do
            var' <- Variable <$> bindName var <*> lookupType var
            return $ Let (NonRec var' (Var scrut)) successBranch
        HS.PLit _ _sign lit -> do
            alt <- Alt (LitPat $ convertLiteral lit)
                <$> pure successBranch
            return $ Case (Var scrut) scrut failBranch [alt]

convertAlt :: HS.Alt Origin -> M Alt
convertAlt alt =
    case alt of
        HS.Alt _ (HS.PApp _ name pats) rhs Nothing -> do
            args <- sequence [ Variable <$> bindName var <*> lookupType var
                             | HS.PVar _ var <- pats ]
            Alt <$> (ConPat <$> resolveQGlobalName name <*> pure args)
                <*> convertRhs rhs
        HS.Alt _ (HS.PTuple _ HS.Unboxed pats) rhs Nothing -> do
            args <- sequence [ Variable <$> bindName var <*> lookupType var
                             | HS.PVar _ var <- pats ]
            Alt (UnboxedPat args)
                <$> convertRhs rhs
        HS.Alt _ (HS.PLit _ _sign lit) rhs Nothing ->
            Alt (LitPat $ convertLiteral lit)
                <$> convertRhs rhs
        -- HS.Alt _ (HS.PVar _ var) rhs Nothing ->
        --     Alt <$> (VarPat <$> (Variable <$> bindName var <*> lookupType var))
        --         <*> convertRhs rhs
        _ -> error $ "convertAlt: " ++ show alt


convertLiteral :: HS.Literal Origin -> Literal
convertLiteral lit =
    case lit of
        HS.PrimString _ str _ -> LitString str
        HS.PrimInt _ int _    -> LitInt int
        HS.PrimChar _ char _  -> LitChar char
        _ -> error "convertLiteral"

toGlobalName :: HS.QName Origin -> GlobalName
toGlobalName qname =
    case info of
        Resolved gname -> gname
        _ -> error $ "toGlobalName: " ++ show qname
  where
    Origin info _ = HS.ann qname

exprType :: Expr -> M TcType
exprType expr =
    case expr of
        Var v -> return (varType v)
        App a b -> do
            aType <- exprType a
            case aType of
                TcFun _ ret -> return ret
                TcForall _ (_ :=> TcFun _ ret) -> return ret
                _ -> return TcUndefined
        WithCoercion _ e -> exprType e
        Let _ e -> exprType e
        LetStrict _ _ e -> exprType e
        Case _ _ (Just e) _ -> exprType e
        Case _ _ Nothing (Alt _ e:_) -> exprType e
        _ -> return TcUndefined

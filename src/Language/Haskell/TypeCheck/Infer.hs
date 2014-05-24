module Language.Haskell.TypeCheck.Infer where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Graph
import           Data.List
import           Language.Haskell.Exts.Annotated.Syntax
import           Language.Haskell.Exts.SrcLoc

import           Language.Haskell.Scope
import           Language.Haskell.TypeCheck.Monad
import           Language.Haskell.TypeCheck.Types


tiGuardedAlts :: GuardedAlts Scoped -> TI TcType
tiGuardedAlts galts =
    case galts of
        UnGuardedAlt _ branch -> tiExp branch
        _ -> error "tiGuardedAlts"

tiAlt :: TcType -> Alt Scoped -> TI TcType
tiAlt scrutTy (Alt _ pat alts Nothing) = do
    patTy <- tiPat pat
    unify scrutTy patTy
    tiGuardedAlts alts

tiExp :: Exp Scoped -> TI TcType
tiExp expr =
    case expr of

        Case _ scrut alts -> do

            ty <- TcMetaVar <$> newTcVar
            scrutTy <- tiExp scrut
            altTys <- mapM (tiAlt scrutTy) alts
            mapM_ (unify ty) altTys
            return ty
        Var _ qname -> do
            let Scoped (Variable src) _ = ann qname
            tySig <- findAssumption src
            _preds :=> ty <- freshInst tySig
            return ty
        Con _ qname -> do
            let Scoped (Global gname) _ = ann qname
            tySig <- findGlobal gname
            _preds :=> ty <- freshInst tySig
            return ty
        App _ fn a -> do
            ty <- TcMetaVar <$> newTcVar
            fnT <- tiExp fn
            aT <- tiExp a
            unify (TcFun aT ty) fnT
            return ty
        Paren _ e -> tiExp e
        -- \a b c -> d
        -- :: a -> b -> c -> d
        Lambda _ pats e -> do
            eTy <- tiExp e
            patTys <- mapM tiPat pats
            return $ foldr TcFun eTy patTys
        _ -> error $ "tiExp: " ++ show expr

tiPat :: Pat Scoped -> TI TcType
tiPat pat =
    case pat of
        PVar _ name -> do
            tv <- TcMetaVar <$> newTcVar
            let Scoped (Variable src) _ = ann name
            setAssumption src tv
            return tv
        PApp _ con pats -> do
            ty <- TcMetaVar <$> newTcVar
            let Scoped (Global gname) _ = ann con
            conSig <- findGlobal gname
            _preds :=> conTy <- freshInst conSig
            patTys <- mapM tiPat pats
            unify conTy (foldr TcFun ty patTys)
            return ty
        PWildCard _ -> do
            ty <- TcMetaVar <$> newTcVar
            return ty
        PParen _ sub ->
            tiPat sub
        _ -> error $ "tiPat: " ++ show pat

tiRhs :: Rhs Scoped -> TI TcType
tiRhs rhs =
    case rhs of
        UnGuardedRhs _ expr ->
            tiExp expr
        _ -> error "tiRhs"

tiMatch :: Match Scoped -> TI TcType
tiMatch match =
    case match of
        -- FIXME: typecheck the binds
        Match _ _ pats rhs Nothing -> do
            patTys <- mapM tiPat pats
            t <- tiRhs rhs
            return $ foldr TcFun t patTys

--matchPatterns :: Match l -> Int
--matchPatterns (Match _ _ paths _ _) = length paths
--matchPatterns InfixMatch{} = 2

tiDecl :: Decl Scoped -> TcType -> TI ()
tiDecl decl ty =
    case decl of
        FunBind _ matches -> do
            ts <- mapM tiMatch matches
            mapM_ (unify ty) ts
        _ -> error "tiDecl"

tiExpl :: Decl Scoped -> TcType -> TI ()
tiExpl decl declType = do
    _preds :=> ty <- freshInst declType
    tiDecl decl ty

declIdent :: Decl Scoped -> SrcLoc
declIdent decl =
    case decl of
        FunBind _ (Match _ name _ _ _:_) ->
            let Scoped _ src = ann name
            in getPointLoc src
        _ -> error "declIdent"

tiImpls :: [Decl Scoped] -> TI ()
tiImpls impls = do
    forM_ impls $ \impl -> do
        ty <- TcMetaVar <$> newTcVar
        setAssumption (declIdent impl) ty
        tiDecl impl ty
        rTy <- zonk ty
        liftIO $ print rTy
    -- qualify the type sigs...

declHeadType :: DeclHead Scoped -> ([TcVar], TcType)
declHeadType dhead =
    case dhead of
        DHead _ name tyVarBinds ->
            let Scoped (Global gname) _ = ann name
                tcVars = map tcVarFromTyVarBind tyVarBinds
            in (tcVars, foldl TcApp (TcCon gname) (map TcVar tcVars))
  where
    tcVarFromTyVarBind (KindedVar _ name _) = tcVarFromName name
    tcVarFromTyVarBind (UnkindedVar _ name) = tcVarFromName name

tiConDecl :: ConDecl Scoped -> (GlobalName, [TcType])
tiConDecl conDecl =
    case conDecl of
        ConDecl _ con bangTys ->
            let Scoped (Global gname) _ = ann con
            in (gname, map getTcType bangTys)

        _ -> error "tiConDecl"
  where
    getTcType (BangedTy _ ty) = typeToTcType ty
    getTcType (UnBangedTy _ ty) = typeToTcType ty
    getTcType (UnpackedTy _ ty) = typeToTcType ty

tiQualConDecl :: QualConDecl Scoped -> (GlobalName, [TcType])
tiQualConDecl (QualConDecl _ _ _ con) =
    tiConDecl con

tiClassDecl :: Decl Scoped -> TI ()
tiClassDecl decl =
    case decl of
        ClassDecl _ _ctx (DHead _ className [tyBind]) _deps (Just decls) ->
            sequence_
                [ worker className tyBind name ty
                | ClsDecl _ (TypeSig _ names ty) <- decls, name <- names ]
  where
    -- tcVarFromName :: Name Scoped -> TcVar
    tcVarFromTyVarBind (KindedVar _ name _) = tcVarFromName name
    tcVarFromTyVarBind (UnkindedVar _ name) = tcVarFromName name
    worker className tyBind name ty = do
        -- name :: className tybind => ty
        let Scoped (Global gname) _ = ann className
            Scoped (Variable src) _ = ann name
            tcVar = tcVarFromTyVarBind tyBind
            tcType = typeToTcType ty
        let scheme = TcForall [tcVar] ([IsIn gname (TcVar tcVar)] :=> tcType)
        setAssumption src scheme


tiPrepareDecl :: Decl Scoped -> TI ()
tiPrepareDecl decl =
    case decl of
        DataDecl _ _ _ dhead cons _ -> do
            let (tcvars, dataTy) = declHeadType dhead
            forM_ cons $ \qualCon -> do
                let (con, fieldTys) = tiQualConDecl qualCon
                    ty = foldr TcFun dataTy fieldTys
                setGlobal con (TcForall tcvars $ [] :=> ty)
        FunBind{} -> return ()
        TypeDecl{} -> return ()
        _ -> error $ "tiPrepareDecl: " ++ show decl

tiDecls :: [SrcLoc] -> [(Decl Scoped, SrcLoc)] -> TI ()
tiDecls explicitlyTyped decls = do
    liftIO $ print $ map snd decls
    forM_ decls $ \(decl, binder) -> do
        ty <- TcMetaVar <$> newTcVar
        setAssumption binder ty
        tiDecl decl ty
        rTy <- zonk ty
        liftIO $ print rTy

    --error $ "tiDecls: " ++ show decls

-- First go through the declarations and add explicit type signatures to
-- the environment. Then type check the implicit declarations in their
-- strongly connected groups. Lastly, verify the signature of explicitly
-- typed declarations (this includes instance methods).
tiBindGroup :: [Decl Scoped] -> TI ()
tiBindGroup decls = do
    mapM_ tiPrepareDecl decls
    forM_ scc $ tiDecls explicitlyTyped . flattenSCC
  where
    explicitlyTyped = []
    graph =
        [ ((decl, binder), binder, declFreeVariables decl )
        | decl <- decls
        , binder <- declBinders decl ]
    scc = stronglyConnComp graph

-- FIXME: Rename this function. We're not finding free variables, we finding
--        all references. 
declFreeVariables :: Decl Scoped -> [SrcLoc]
declFreeVariables decl =
    case decl of
        FunBind _ matches -> concatMap freeMatch matches
        _ -> error $ "declFreeVariables: " ++ show decl
  where
    freeMatch match =
        case match of
            Match _ _ _pats rhs _binds -> freeRhs rhs
    freeRhs rhs =
        case rhs of
            UnGuardedRhs _ expr -> freeExp expr
    freeExp expr =
        case expr of
            Var _ qname -> [qnameIdentifier qname]
            Con{} -> []
            Case _ scrut alts -> freeExp scrut ++ concatMap freeAlt alts
            App _ a b -> freeExp a ++ freeExp b
            Paren _ e -> freeExp e
            Lambda _ _pats e -> freeExp e
            _ -> error $ "freeExp: " ++ show expr
    freeAlt (Alt _ _pat guarded _binds) =
        case guarded of
            UnGuardedAlt _ expr -> freeExp expr

qnameIdentifier :: QName Scoped -> SrcLoc
qnameIdentifier qname =
    case qname of
        Qual _ _ name -> nameIdentifier name
        UnQual _ name -> nameIdentifier name

nameIdentifier :: Name Scoped -> SrcLoc
nameIdentifier name =
    case info of
        Variable src -> getPointLoc src
        _ -> error "nameIdentifier"
  where
    Scoped info _ = ann name

declBinders :: Decl Scoped -> [SrcLoc]
declBinders decl =
    case decl of
        DataDecl{} -> []
        ForImp{}   -> []
        FunBind _ matches ->
            case head matches of
                Match _ name _ _ _ ->
                    let Scoped _ loc = ann name
                    in [getPointLoc loc]
        TypeDecl{} -> []
        _ -> error $ "declBinders: " ++ show decl

tiModule :: Module Scoped -> TI ()
tiModule m =
    case m of
        Module _ _dhead _pragma _imports decls ->
            tiBindGroup decls

module Language.Haskell.TypeCheck.Infer where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Graph
import           Language.Haskell.Exts.Annotated.Syntax
import           Language.Haskell.Exts.SrcLoc
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

import           Language.Haskell.Scope
import           Language.Haskell.TypeCheck.Monad
import           Language.Haskell.TypeCheck.Types


tiGuardedAlts :: GuardedAlts Origin -> TI TcType
tiGuardedAlts galts =
    case galts of
        UnGuardedAlt _ branch -> tiExp branch
        _ -> error "tiGuardedAlts"

tiAlt :: TcType -> Alt Origin -> TI TcType
tiAlt scrutTy (Alt _ pat alts _mbBinds) = do
    patTy <- tiPat pat
    unify scrutTy patTy
    tiGuardedAlts alts

tiLit :: Literal Origin -> TI TcType
tiLit lit =
    case lit of
        PrimInt{} -> return $ TcCon (mkBuiltIn "Main" "I64")
        PrimString{} -> return $ TcApp
            (TcCon (mkBuiltIn "Main" "Addr"))
            (TcCon (mkBuiltIn "Main" "I8"))
        _ -> error $ "tiLit: " ++ show lit

tiQOp :: QOp Origin -> TI TcType
tiQOp op =
    case op of
        QVarOp src var -> tiExp (Var src var)
        QConOp src con -> tiExp (Con src con)

tiExp :: Exp Origin -> TI TcType
tiExp expr =
    case expr of

        Case _ scrut alts -> do

            ty <- TcMetaVar <$> newTcVar
            scrutTy <- tiExp scrut
            altTys <- mapM (tiAlt scrutTy) alts
            mapM_ (unify ty) altTys
            return ty
        Var _ qname -> do
            let Origin (Resolved (GlobalName src _qname)) _ = ann qname
            tySig <- findAssumption src
            _preds :=> ty <- freshInst tySig
            return ty
        Con _ qname -> do
            let Origin (Resolved gname) _ = ann qname
            tySig <- findGlobal gname
            _preds :=> ty <- freshInst tySig
            return ty
        App _ fn a -> do
            ty <- TcMetaVar <$> newTcVar
            fnT <- tiExp fn
            aT <- tiExp a
            unify (TcFun aT ty) fnT
            return ty
        InfixApp _ a op b -> do
            ty <- TcMetaVar <$> newTcVar
            opT <- tiQOp op
            aT <- tiExp a
            bT <- tiExp b
            unify (TcFun aT (TcFun bT ty)) opT
            return ty
        Paren _ e -> tiExp e
        -- \a b c -> d
        -- :: a -> b -> c -> d
        Lambda _ pats e -> do
            patTys <- mapM tiPat pats
            eTy <- tiExp e
            return $ foldr TcFun eTy patTys
        Lit _ lit -> tiLit lit
        _ -> error $ "tiExp: " ++ show expr

tiPat :: Pat Origin -> TI TcType
tiPat pat =
    case pat of
        PVar _ name -> do
            tv <- TcMetaVar <$> newTcVar
            let Origin (Resolved (GlobalName src _qname)) _ = ann name
            setAssumption src tv
            return tv
        PApp _ con pats -> do
            ty <- TcMetaVar <$> newTcVar
            let Origin (Resolved gname) _ = ann con
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

tiRhs :: Rhs Origin -> TI TcType
tiRhs rhs =
    case rhs of
        UnGuardedRhs _ expr ->
            tiExp expr
        _ -> error "tiRhs"

tiMatch :: Match Origin -> TI TcType
tiMatch match =
    case match of
        -- FIXME: typecheck the binds
        Match _ _ pats rhs Nothing -> do
            patTys <- mapM tiPat pats
            t <- tiRhs rhs
            return $ foldr TcFun t patTys
        _ -> error "tiMatch"

--matchPatterns :: Match l -> Int
--matchPatterns (Match _ _ paths _ _) = length paths
--matchPatterns InfixMatch{} = 2

tiDecl :: Decl Origin -> TcType -> TI ()
tiDecl decl ty =
    case decl of
        FunBind _ matches -> do
            ts <- mapM tiMatch matches
            mapM_ (unify ty) ts
        PatBind _ _pat _ty rhs _binds -> do
            rhsTy <- tiRhs rhs
            unify ty rhsTy
        _ -> error $ "tiDecl: " ++ show decl

--tiExpl :: Decl Origin -> TcType -> TI ()
--tiExpl decl declType = do
--    _preds :=> ty <- freshInst declType
--    tiDecl decl ty

declIdent :: Decl Origin -> SrcLoc
declIdent decl =
    case decl of
        FunBind _ (Match _ name _ _ _:_) ->
            let Origin _ src = ann name
            in getPointLoc src
        _ -> error "declIdent"

--tiImpls :: [Decl Origin] -> TI ()
--tiImpls impls = do
--    forM_ impls $ \impl -> do
--        ty <- TcMetaVar <$> newTcVar
--        setAssumption (declIdent impl) ty
--        tiDecl impl ty
--        rTy <- zonk ty
--        liftIO $ print rTy
    -- qualify the type sigs...

declHeadType :: DeclHead Origin -> ([TcVar], TcType)
declHeadType dhead =
    case dhead of
        DHead _ name tyVarBinds ->
            let Origin (Resolved gname) _ = ann name
                tcVars = map tcVarFromTyVarBind tyVarBinds
            in (tcVars, foldl TcApp (TcCon gname) (map TcRef tcVars))
        _ -> error "declHeadType"
  where
    tcVarFromTyVarBind (KindedVar _ name _) = tcVarFromName name
    tcVarFromTyVarBind (UnkindedVar _ name) = tcVarFromName name

tiConDecl :: [TcVar] -> TcType -> ConDecl Origin -> TI (GlobalName, [TcType])
tiConDecl tvars dty conDecl =
    case conDecl of
        ConDecl _ con bangTys -> do
            let Origin (Resolved gname) _ = ann con
            return (gname, map getTcType bangTys)
        RecDecl _ con fields -> do
            let Origin (Resolved gname) _ = ann con
                conTys = concat
                    [ replicate (length names) (getTcType bangTy)
                    | FieldDecl _ names bangTy <- fields ]
            forM_ fields $ \(FieldDecl _ names bangTy) -> do
                let ty = TcFun dty (getTcType bangTy)
                forM_ names $ \name -> do
                    let Origin (Resolved (GlobalName src _qname)) _ = ann name
                    setAssumption src (TcForall tvars $ [] :=> ty)
            return (gname, conTys)
        _ -> error "tiConDecl"
  where
    getTcType (BangedTy _ ty) = typeToTcType ty
    getTcType (UnBangedTy _ ty) = typeToTcType ty
    getTcType (UnpackedTy _ ty) = typeToTcType ty

tiQualConDecl :: [TcVar] -> TcType -> QualConDecl Origin -> TI (GlobalName, [TcType])
tiQualConDecl tvars dty (QualConDecl _ _ _ con) =
    tiConDecl tvars dty con

tiClassDecl :: Decl Origin -> TI ()
tiClassDecl decl =
    case decl of
        ClassDecl _ _ctx (DHead _ className [tyBind]) _deps (Just decls) ->
            sequence_
                [ worker className tyBind name ty
                | ClsDecl _ (TypeSig _ names ty) <- decls, name <- names ]
        _ -> error "tiClassDecl"
  where
    -- tcVarFromName :: Name Origin -> TcVar
    tcVarFromTyVarBind (KindedVar _ name _) = tcVarFromName name
    tcVarFromTyVarBind (UnkindedVar _ name) = tcVarFromName name
    worker className tyBind name ty = do
        -- name :: className tybind => ty
        let Origin (Resolved gname) _ = ann className
            Origin (Resolved (GlobalName src _qname)) _ = ann name
            tcVar = tcVarFromTyVarBind tyBind
            tcType = typeToTcType ty
        let scheme = TcForall [tcVar] ([IsIn gname (TcRef tcVar)] :=> tcType)
        setAssumption src scheme


tiPrepareDecl :: Decl Origin -> TI ()
tiPrepareDecl decl =
    case decl of
        DataDecl _ _ _ dhead cons _ -> do
            let (tcvars, dataTy) = declHeadType dhead
            forM_ cons $ \qualCon -> do
                (con, fieldTys) <- tiQualConDecl tcvars dataTy qualCon
                let ty = foldr TcFun dataTy fieldTys
                setGlobal con (TcForall tcvars $ [] :=> ty)
        FunBind{} -> return ()
        PatBind{} -> return ()
        TypeDecl{} -> return ()
        ForImp _ _conv _safety _mbExternal name ty -> do
            let Origin _ src = ann name
            setAssumption src (typeToTcType ty)
        TypeSig _ names ty -> do
            forM_ names $ \name -> do
                setAssumption (nameIdentifier name) (explicitTcForall $ typeToTcType ty)
        _ -> error $ "tiPrepareDecl: " ++ show decl

tiExpl :: (Decl Origin, SrcSpanInfo) -> TI ()
tiExpl (decl, binder) = do
    ty <- TcMetaVar <$> newTcVar
    tiDecl decl ty
    tySig <- findAssumption binder
    _preds :=> expected <- freshInst tySig
    unify ty expected

tiDecls :: [(Decl Origin, SrcSpanInfo)] -> TI ()
tiDecls decls = do
    free <- getFreeMetaVariables
    liftIO $ print $ map snd decls
    forM_ decls $ \(decl, binder) -> do
        ty <- TcMetaVar <$> newTcVar
        setAssumption binder ty
    forM_ decls $ \(decl, binder) -> do
        ty <- findAssumption binder
        tiDecl decl ty
    forM_ decls $ \(decl, binder) -> do
        ty <- findAssumption binder
        rTy <- zonk ty
        liftIO $ print $ Doc.pretty rTy
        gTy <- generalize free rTy
        liftIO $ print $ Doc.pretty gTy
        setAssumption binder gTy


    --error $ "tiDecls: " ++ show decls

-- First go through the declarations and add explicit type signatures to
-- the environment. Then type check the implicit declarations in their
-- strongly connected groups. Lastly, verify the signature of explicitly
-- typed declarations (this includes instance methods).
tiBindGroup :: [Decl Origin] -> TI ()
tiBindGroup decls = do
    liftIO $ putStrLn $ "Explicit: " ++ show explicitlyTyped
    mapM_ tiPrepareDecl decls
    forM_ scc $ tiDecls . flattenSCC
    forM_ explicitDecls tiExpl
  where
    explicitlyTyped =
        [ nameIdentifier name
        | TypeSig _ names _ <- decls
        , name <- names ]
    explicitDecls =
        [ (decl, binder)
        | decl <- decls
        , binder <- declBinders decl
        , binder `elem` explicitlyTyped ]
    graph =
        [ ((decl, binder), binder, declFreeVariables decl )
        | decl <- decls
        , binder <- declBinders decl
        , binder `notElem` explicitlyTyped ]
    scc = stronglyConnComp graph

-- FIXME: Rename this function. We're not finding free variables, we finding
--        all references. 
declFreeVariables :: Decl Origin -> [SrcSpanInfo]
declFreeVariables decl =
    case decl of
        FunBind _ matches -> concatMap freeMatch matches
        PatBind _ _pat _ty rhs _binds -> freeRhs rhs
        _ -> error $ "declFreeVariables: " ++ show decl
  where
    freeMatch match =
        case match of
            Match _ _ _pats rhs _binds -> freeRhs rhs
            _ -> error "declFreeVariables, freeMatch"
    freeRhs rhs =
        case rhs of
            UnGuardedRhs _ expr -> freeExp expr
            _ -> error "declFreeVariables, freeRhs"
    freeExp expr =
        case expr of
            Var _ qname -> [qnameIdentifier qname]
            Con{} -> []
            Lit{} -> []
            Case _ scrut alts -> freeExp scrut ++ concatMap freeAlt alts
            App _ a b -> freeExp a ++ freeExp b
            InfixApp _ a op b -> freeExp a ++ freeQOp op ++ freeExp b
            Paren _ e -> freeExp e
            Lambda _ _pats e -> freeExp e
            _ -> error $ "freeExp: " ++ show expr
    freeQOp op =
        case op of
            QVarOp _ qname -> [qnameIdentifier qname]
            QConOp{} -> []
    freeAlt (Alt _ _pat guarded _binds) =
        case guarded of
            UnGuardedAlt _ expr -> freeExp expr
            _ -> error "declFreeVariables, freeAlt"

qnameIdentifier :: QName Origin -> SrcSpanInfo
qnameIdentifier qname =
    case qname of
        Qual _ _ name -> nameIdentifier name
        UnQual _ name -> nameIdentifier name
        _ -> error "qnameIdentifier"

nameIdentifier :: Name Origin -> SrcSpanInfo
nameIdentifier name =
    case info of
        Resolved (GlobalName src _qname) -> src
        _ -> error "nameIdentifier"
  where
    Origin info _ = ann name

declBinders :: Decl Origin -> [SrcSpanInfo]
declBinders decl =
    case decl of
        DataDecl{} -> []
        ForImp{}   -> []
        FunBind _ matches ->
            case head matches of
                Match _ name _ _ _ ->
                    let Origin _ loc = ann name
                    in [loc]
                _ -> error "declBinders, FunBind"
        PatBind _ pat _ty _rhs _binds ->
            patBinders pat
        TypeDecl{} -> []
        TypeSig{} -> []
        _ -> error $ "declBinders: " ++ show decl

patBinders :: Pat Origin -> [SrcSpanInfo]
patBinders pat =
    case pat of
        PVar _ name ->
            let Origin _ loc = ann name
            in [loc]
        _ -> error $ "patBinders: " ++ show pat

tiModule :: Module Origin -> TI ()
tiModule m =
    case m of
        Module _ _dhead _pragma _imports decls ->
            tiBindGroup decls
        _ -> error "tiModule"

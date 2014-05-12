module Language.Haskell.TypeCheck.Infer where

import Data.List
import Control.Monad
import Control.Monad.Trans
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.SrcLoc
import Control.Applicative

import Language.Haskell.Scope
import Language.Haskell.TypeCheck.Types
import Language.Haskell.TypeCheck.Monad


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
        _ -> error "tiExp"

tiPat :: Pat Scoped -> TI TcType
tiPat pat =
    case pat of
        PVar _ name -> do
            tv <- TcMetaVar <$> newTcVar
            let Scoped (Variable src) _ = ann name
            setAssumption src (Scheme [] $ [] :=> tv)
            return tv
        PApp _ con pats -> do
            ty <- TcMetaVar <$> newTcVar
            let Scoped (Global gname) _ = ann con
            conSig <- findGlobal gname
            _preds :=> conTy <- freshInst conSig
            patTys <- mapM tiPat pats
            unify ty (foldl TcApp conTy patTys)
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

tiExpl :: Decl Scoped -> Scheme -> TI ()
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
        setAssumption (declIdent impl) (Scheme [] $ [] :=> ty)
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

tiTypeDecl :: Decl Scoped -> TI ()
tiTypeDecl decl =
    case decl of
        DataDecl _ _ _ dhead cons _ -> do
            let (tcvars, dataTy) = declHeadType dhead
            forM_ cons $ \qualCon -> do
                let (con, fieldTys) = tiQualConDecl qualCon
                    ty = foldr TcFun dataTy fieldTys
                setGlobal con (Scheme tcvars $ [] :=> ty)
            --setGlobal Nothing (forall a. Maybe a)
            --setGlobal Just (forall a. a -> Maybe a)
        _ -> error "tiTypeDecl"

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
        let scheme = Scheme [tcVar] ([IsIn gname (TcVar tcVar)] :=> tcType)
        setAssumption src scheme



-- Split decls into class decls, data decls, expl decl and impl decl
tiBindGroup :: [Decl Scoped] -> TI ()
tiBindGroup decls = do
    mapM_ tiClassDecl classDecls
    mapM_ tiTypeDecl typeDecls
    tiImpls implDecls
    -- mapM_ tiExpl explDecls
  where
    (typeDecls, decls') = partition isTypeDecl decls
    (classDecls, decls'') = partition isClassDecl decls'
    implDecls = decls''
    isTypeDecl DataDecl{} = True
    isTypeDecl TypeDecl{} = True
    isTypeDecl _ = False
    isClassDecl ClassDecl{} = True
    isClassDecl _ = False
tiModule :: Module Scoped -> TI ()
tiModule m =
    case m of
        Module _ _dhead _pragma _imports decls ->
            tiBindGroup decls

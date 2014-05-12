{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Scope where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe
import           Language.Haskell.Exts.Annotated        (ParseResult (..),
                                                         parseFile)
import           Language.Haskell.Exts.Annotated.Syntax
import           Language.Haskell.Exts.SrcLoc
import           Prelude                                hiding (span)

data Scoped = Scoped NameInfo SrcSpanInfo
    deriving ( Show )
-- TypeVars are uniquely identified by their SrcLoc and their name.
-- Variables are uniquely identified by their SrcLoc
-- Globals are uniquely identified by their GlobalName and namespace
data NameInfo
    = TypeVar SrcLoc
    | Global GlobalName
    | Variable SrcLoc
    | None
    | ScopeError ScopeError
    deriving ( Show )

data ScopeError
    = ENotInScope SrcSpanInfo
    | ETyVarNotInScope String SrcSpanInfo
    | EGlobalNotInScope GlobalName SrcSpanInfo
    | EAmbiguous
    | ETypeAsClass
    | ENotExported
    | EModNotFound
    | EInternal
    deriving ( Show )

testResolve :: IO ()
testResolve = do
    ParseOk m <- parseFile "src/Test.hs"
    let (errs, _m') = resolve m
    mapM_ print errs

-- Resolve all names in a module
resolve :: Module SrcSpanInfo -> ([ScopeError], Module Scoped)
resolve m = runRename $ resolveModule m




-----------------------------------------------------------
-- Types and Monad

data GlobalName = GlobalName
    { gnameModule     :: String
    , gnameIdentifier :: String }
    deriving ( Eq, Ord, Show )
data Namespace
    = NsTypes
    | NsConstructors
    | NsVariables
    | NsTypeVariables
    deriving ( Show )
data Scope = Scope
    { scopeTvRoot       :: Maybe SrcLoc
    , scopeModuleName   :: String
    , scopeTypes        :: Map GlobalName GlobalName
    , scopeConstructors :: Map GlobalName GlobalName
    , scopeTyVars       :: Map String SrcLoc
    , scopeVariables    :: Map GlobalName SrcLoc
    , scopeErrors       :: [ScopeError]
    }
instance Monoid Scope where
    mempty = Scope
        { scopeTvRoot       = Nothing
        , scopeModuleName   = "Main"
        , scopeTypes        = Map.empty
        , scopeConstructors = Map.empty
        , scopeTyVars       = Map.empty
        , scopeVariables    = Map.empty
        , scopeErrors       = [] }
    -- FIXME: Catch ambiguous definitions.
    mappend a b = Scope
        { scopeTvRoot = scopeTvRoot a
        , scopeModuleName = scopeModuleName a
        , scopeTypes = scopeTypes a `Map.union` scopeTypes b
        , scopeConstructors = scopeConstructors a `Map.union` scopeConstructors b
        , scopeTyVars = scopeTyVars a `Map.union` scopeTyVars b
        , scopeVariables = scopeVariables a `Map.union` scopeVariables b
        , scopeErrors = scopeErrors a ++ scopeErrors b }
newtype Rename a = Rename { unRename :: ReaderT Scope (Writer Scope) a }
    deriving
        ( Monad, MonadReader Scope, MonadWriter Scope
        , Functor, Applicative )

type Resolve a = a SrcSpanInfo -> Rename (a Scoped)

-----------------------------------------------------------
-- Utilities

runRename :: Rename a -> ([ScopeError], a)
runRename action = (scopeErrors scope, a)
  where
    (a, scope) = runWriter (runReaderT (unRename action) scope)

getTvRoot :: Rename (Maybe SrcLoc)
getTvRoot = asks scopeTvRoot

withTvRoot :: SrcLoc -> Rename a -> Rename a
withTvRoot root = local $ \env -> env{ scopeTvRoot = Just root }

-- Run action without letting the tyVars escsape the scope.
limitTyVarScope :: Rename a -> Rename a
limitTyVarScope action = Rename $ ReaderT $ \scope ->
    let inScope = scope{ scopeTyVars = scopeTyVars scope `Map.union` scopeTyVars nestedScope}
        (a, nestedScope) = runWriter $ runReaderT (unRename action) inScope
    in WriterT $ Identity (a, nestedScope{ scopeTyVars = Map.empty })

limitScope :: Rename a -> Rename a
limitScope action = Rename $ ReaderT $ \scope ->
    let inScope = (scope `mappend` nestedScope){ scopeErrors = []}
        (a, nestedScope) = runWriter $ runReaderT (unRename action) inScope
    in WriterT $ Identity (a, mempty{ scopeErrors = scopeErrors nestedScope})

qnameToGlobalName :: QName l -> GlobalName
qnameToGlobalName qname =
    case qname of
        Qual _ (ModuleName _ m) name -> GlobalName m (getNameIdentifier name)
        UnQual _ name -> GlobalName "" (getNameIdentifier name)
        Special{} -> error "qnameToGlobalName"

getNameIdentifier :: Name l -> String
getNameIdentifier (Ident _ ident) = ident
getNameIdentifier (Symbol _ symbol) = symbol

resolveName :: Namespace -> Resolve Name
resolveName ns name =
    case name of
        Ident span ident ->
            Ident
                <$> getScoped span ident
                <*> pure ident
        Symbol span symbol -> do
            Symbol
                <$> getScoped span symbol
                <*> pure symbol
  where
    worker field nsType span gname = do
        m <- asks field
        let ret = case Map.lookup gname m of
                Nothing -> Left (ENotInScope span)
                Just var -> Right var
            nameInfo = either ScopeError nsType ret
        tell mempty{ scopeErrors = maybeToList (either (Just) (const Nothing) ret) }
        return $ Scoped nameInfo span
    mkGlobal = GlobalName ""
    getScoped span =
        case ns of
            NsVariables     -> worker scopeVariables Variable span . mkGlobal
            NsConstructors  -> worker scopeConstructors Global span . mkGlobal
            NsTypes         -> worker scopeTypes Global span . mkGlobal
            NsTypeVariables -> worker scopeTyVars TypeVar span

defineType :: Resolve Name
defineType name = do
    thisModule <- asks scopeModuleName
    let gname = GlobalName "" (getNameIdentifier name)
        resolved = GlobalName thisModule (getNameIdentifier name)
    tell mempty{ scopeTypes = Map.singleton gname resolved}
    resolveName NsTypes name

defineConstructor :: Resolve Name
defineConstructor name = do
    thisModule <- asks scopeModuleName
    let gname = GlobalName "" (getNameIdentifier name)
        resolved = GlobalName thisModule (getNameIdentifier name)
    tell mempty{ scopeConstructors = Map.singleton gname resolved }
    resolveName NsConstructors name

-- FIXME: merge defineVariable, defineConstructor, defineType, defineTyVar
-- using the same pattern as resolveName.
defineVariable :: Resolve Name
defineVariable name = do
    let point = getPointLoc (ann name)
        gname = GlobalName "" (getNameIdentifier name)
    tell mempty{ scopeVariables = Map.singleton gname point}
    resolveName NsVariables name

defineTyVar :: Resolve Name
defineTyVar name = do
    let span = ann name
        ident = getNameIdentifier name
    tell mempty{ scopeTyVars = Map.singleton ident (getPointLoc span) }
    resolveName NsTypeVariables name

-- resolveMaybe :: Resolve a -> Resolve (Maybe a)
resolveMaybe :: (a -> Rename b) -> Maybe a -> Rename (Maybe b)
resolveMaybe fn mbValue =
    case mbValue of
        Nothing    -> return Nothing
        Just value -> Just <$> fn value

freeVariables :: Type SrcSpanInfo -> [String]
freeVariables = worker []
  where
    worker ignore (TyForall _ Nothing _ ty) = worker ignore ty
    worker ignore (TyForall _ (Just binds) _ ty) =
        worker (map getTyVar binds ++ ignore) ty
    worker ignore (TyFun _ a b) = worker ignore a ++ worker ignore b
    worker ignore (TyApp _ a b) = worker ignore a ++ worker ignore b
    worker ignore (TyVar _ var)
        | getNameIdentifier var `elem` ignore = []
        | otherwise = [getNameIdentifier var]
    worker _ TyCon{} = []
    worker _ ty = error $ "freeVariables: " ++ show ty
    getTyVar (KindedVar _ name _) = getNameIdentifier name
    getTyVar (UnkindedVar _ name) = getNameIdentifier name

-----------------------------------------------------------
-- Name resolution

resolveSafety :: Resolve Safety
resolveSafety safety =
    case safety of
        PlayRisky src  ->
            pure $ PlayRisky (Scoped None src)
        PlaySafe src b ->
            pure $ PlaySafe (Scoped None src) b
        PlayInterruptible src ->
            pure $ PlayInterruptible (Scoped None src)

resolveCallConv :: Resolve CallConv
resolveCallConv conv = pure $
    case conv of
        StdCall src   -> StdCall (Scoped None src)
        CCall src     -> CCall (Scoped None src)
        CPlusPlus src -> CPlusPlus (Scoped None src)
        DotNet src    -> DotNet (Scoped None src)
        Jvm src       -> Jvm (Scoped None src)
        Js src        -> Js (Scoped None src)
        CApi src      -> CApi (Scoped None src)

resolveKind :: Resolve Kind
resolveKind kind =
    case kind of
        KindStar span -> pure $ KindStar (Scoped None span)
        _ -> error "resolveKind"

resolveTyVarBind :: Resolve TyVarBind
resolveTyVarBind tyVarBind =
    case tyVarBind of
        KindedVar span name kind ->
            KindedVar
                <$> pure (Scoped None span)
                <*> defineTyVar name
                <*> resolveKind kind
        UnkindedVar span name    ->
            UnkindedVar
                <$> pure (Scoped None span)
                <*> defineTyVar name

resolveDeclHead :: Resolve DeclHead
resolveDeclHead dhead =
    case dhead of
        DHead span name tyVarBinds ->
            DHead
                <$> pure (Scoped None span)
                <*> defineType name
                <*> mapM resolveTyVarBind tyVarBinds
        DHInfix{} -> error "resolveDeclHead"
        DHParen _ next -> resolveDeclHead next

resolveContext :: Resolve Context
resolveContext ctx =
    case ctx of
        _ -> error "resolveContext"

resolveDataOrNew :: Resolve DataOrNew
resolveDataOrNew dataOrNew =
    case dataOrNew of
        DataType span -> pure $ DataType (Scoped None span)
        NewType span  -> pure $ NewType (Scoped None span)

resolveInstHead :: Resolve InstHead
resolveInstHead instHead =
    case instHead of
        IHead src className tys ->
            IHead (Scoped None src)
                <$> resolveQName NsTypes className
                <*> mapM resolveType tys
        IHInfix{} -> error "resolveInstHead"
        IHParen span sub ->
            IHParen (Scoped None span)
                <$> resolveInstHead sub

resolveDeriving :: Resolve Deriving
resolveDeriving (Deriving span instHeads) =
    Deriving
        <$> pure (Scoped None span)
        <*> mapM resolveInstHead instHeads

resolveSpecialCon :: Resolve SpecialCon
resolveSpecialCon specialCon = pure $
    case specialCon of
        UnitCon src             -> UnitCon $ Scoped None src
        ListCon src             -> ListCon $ Scoped None src
        FunCon src              -> FunCon $ Scoped None src
        TupleCon src boxed size -> TupleCon (Scoped None src) boxed size
        Cons src                -> Cons $ Scoped None src
        UnboxedSingleCon src    -> UnboxedSingleCon $ Scoped None src

resolveQName :: Namespace -> Resolve QName
resolveQName ns qname =
    case qname of
        Qual span (ModuleName l m) name ->
            Qual
                <$> getScoped span (GlobalName m (getNameIdentifier name))
                <*> pure (ModuleName (Scoped None l) m)
                <*> resolveName ns name
        UnQual span name ->
            UnQual
                <$> getScoped span (GlobalName "" (getNameIdentifier name))
                <*> resolveName ns name
        Special src specialCon ->
            Special (Scoped None src)
                <$> resolveSpecialCon specialCon
  where
    worker field nsType span gname = do
        m <- asks field
        let ret = case Map.lookup gname m of
                Nothing -> Left (EGlobalNotInScope gname span)
                Just var -> Right var
            nameInfo = either ScopeError nsType ret
        tell mempty{ scopeErrors = maybeToList (either (Just) (const Nothing) ret) }
        return $ Scoped nameInfo span
    getScoped =
        case ns of
            NsVariables    -> worker scopeVariables Variable
            NsConstructors -> worker scopeConstructors Global
            NsTypes        -> worker scopeTypes Global

-- FIXME: Yikes, merge this with resolveName
resolveTyVar :: Resolve Name
resolveTyVar tyVar =
    case tyVar of
        Ident span ident ->
            Ident
                <$> getScoped span ident
                <*> pure ident
        Symbol span symbol ->
            Symbol
                <$> getScoped span symbol
                <*> pure symbol
  where
    getScoped span key = do
        m <- asks scopeTyVars
        mbRoot <- getTvRoot
        let mbError = case Map.lookup key m of
                Nothing -> case mbRoot of
                    Nothing -> Just (ETyVarNotInScope key span)
                    Just{} -> Nothing{}
                Just{} -> Nothing
        tell mempty{ scopeErrors = maybeToList mbError }
        pure $ Scoped (
            case Map.lookup key m of
                Nothing -> case mbRoot of
                    Nothing -> ScopeError (ETyVarNotInScope key span)
                    Just point -> TypeVar point
                Just point -> TypeVar point) span

resolveType :: Resolve Type
resolveType ty =
    case ty of
        TyCon span qname ->
            TyCon (Scoped None span)
                <$> resolveQName NsTypes qname
        TyVar span tyVar ->
            TyVar (Scoped None span)
                <$> resolveTyVar tyVar
        TyFun span a b ->
            TyFun (Scoped None span)
                <$> resolveType a
                <*> resolveType b
        TyApp span a b ->
            TyApp (Scoped None span)
                <$> resolveType a
                <*> resolveType b
        TyParen src sub ->
            TyParen (Scoped None src)
                <$> resolveType sub
        TyTuple src boxed tys ->
            TyTuple (Scoped None src) boxed
                <$> mapM resolveType tys
        _ -> error $ "resolveType: " ++ show ty

resolveBangType :: Resolve BangType
resolveBangType bangTy =
    case bangTy of
        BangedTy span ty ->
            BangedTy (Scoped None span)
                <$> resolveType ty
        UnBangedTy span ty ->
            UnBangedTy (Scoped None span)
                <$> resolveType ty
        UnpackedTy span ty ->
            UnpackedTy (Scoped None span)
                <$> resolveType ty

resolveConDecl :: Resolve ConDecl
resolveConDecl conDecl =
    case conDecl of
        ConDecl span name bangTys ->
            ConDecl (Scoped None span)
                <$> defineConstructor name
                <*> mapM resolveBangType bangTys

resolveQualConDecl :: Resolve QualConDecl
resolveQualConDecl (QualConDecl span mbTyVarBinds ctx conDecl) =
    QualConDecl (Scoped None span)
        <$> resolveMaybe (mapM resolveTyVarBind) mbTyVarBinds
        <*> resolveMaybe resolveContext ctx
        <*> resolveConDecl conDecl

resolvePat :: Resolve Pat
resolvePat pat =
    case pat of
        PVar span name ->
            PVar (Scoped None span)
                <$> defineVariable name
        PApp span con pats ->
            PApp (Scoped None span)
                <$> resolveQName NsConstructors con
                <*> mapM resolvePat pats
        PWildCard span ->
            pure $ PWildCard (Scoped None span)
        PParen span sub ->
            PParen (Scoped None span)
                <$> resolvePat sub
        PTuple src boxed pats ->
            PTuple (Scoped None src) boxed
                <$> mapM resolvePat pats
        _ -> error $ "resolvePat: " ++ show pat

resolveGuardedAlts :: Resolve GuardedAlts
resolveGuardedAlts galts =
    case galts of
        UnGuardedAlt span expr ->
            UnGuardedAlt (Scoped None span)
                <$> resolveExp expr
        _ -> error "resolveGuardedAlts"

resolveAlt :: Resolve Alt
resolveAlt (Alt span pat guarded mbBinds) = limitScope $
    Alt (Scoped None span)
        <$> resolvePat pat
        <*> resolveGuardedAlts guarded
        <*> resolveMaybe undefined mbBinds

resolveQOp :: Resolve QOp
resolveQOp qop =
    case qop of
        QVarOp src qname ->
            QVarOp (Scoped None src)
                <$> resolveQName NsVariables qname
        QConOp src qname ->
            QConOp (Scoped None src)
                <$> resolveQName NsConstructors qname

resolveLiteral :: Resolve Literal
resolveLiteral lit = pure $
    case lit of
        Char src c orig   -> worker Char src c orig
        String src s orig -> worker String src s orig
        Int src i orig    -> worker Int src i orig
        Frac src r orig   -> worker Frac src r orig
        PrimInt src i orig -> worker PrimInt src i orig
        PrimWord src w orig -> worker PrimWord src w orig
        PrimFloat src f orig -> worker PrimFloat src f orig
        PrimDouble src d orig -> worker PrimDouble src d orig
        PrimChar src c orig -> worker PrimChar src c orig
        PrimString src s orig -> worker PrimString src s orig
  where
    worker con src =
        con (Scoped None src)

resolveExp :: Resolve Exp
resolveExp expr =
    case expr of
        Case span scrut alts ->
            Case (Scoped None span)
                <$> resolveExp scrut
                <*> mapM resolveAlt alts
        Con span qname ->
            Con (Scoped None span)
                <$> resolveQName NsConstructors qname
        Var span qname ->
            Var (Scoped None span)
                <$> resolveQName NsVariables qname
        App src a b ->
            App (Scoped None src)
                <$> resolveExp a
                <*> resolveExp b
        InfixApp src a qop b ->
            InfixApp (Scoped None src)
                <$> resolveExp a
                <*> resolveQOp qop
                <*> resolveExp b
        Paren src sub ->
            Paren (Scoped None src)
                <$> resolveExp sub
        Lambda src pats sub -> limitScope $
            Lambda (Scoped None src)
                <$> mapM resolvePat pats
                <*> resolveExp sub
        Lit src lit ->
            Lit (Scoped None src)
                <$> resolveLiteral lit
        Tuple src boxed exps ->
            Tuple (Scoped None src) boxed
                <$> mapM resolveExp exps
        _ -> error $ "resolveExp: " ++ show expr

resolveRhs :: Resolve Rhs
resolveRhs rhs =
    case rhs of
        UnGuardedRhs span expr ->
            UnGuardedRhs (Scoped None span)
                <$> resolveExp expr
        _ -> error "resolveRhs"

resolveMatch :: Resolve Match
resolveMatch match =
    case match of
        Match span name pats rhs mbBinds -> limitScope $
            Match (Scoped None span)
                <$> resolveName NsVariables name
                <*> mapM resolvePat pats
                <*> resolveRhs rhs
                <*> resolveMaybe undefined mbBinds
        _ -> error "resolveMatch"

-- FIXME: Move this
matchName :: Match l -> Name l
matchName (Match _span name _pats _rhs _binds) = name
matchName (InfixMatch _span _left name _right _rhs _binds) = name

resolveClassDecl :: Resolve ClassDecl
resolveClassDecl decl =
    case decl of
        ClsDecl src sub ->
          ClsDecl (Scoped None src)
            <$> resolveDecl ResolveClass sub
        _ -> error "resolveClassDecl"

resolveFunDep :: Resolve FunDep
resolveFunDep dependency =
    case dependency of
        _ -> error "resolveFunDep"

resolveInstDecl :: Resolve InstDecl
resolveInstDecl inst =
    case inst of
        InsDecl src decl ->
            InsDecl (Scoped None src)
                <$> resolveDecl ResolveInstance decl
        _ -> error "resolveInstDecl"

resolveActivation :: Resolve Activation
resolveActivation activation = pure $
    case activation of
        ActiveFrom src n -> ActiveFrom (Scoped None src) n
        ActiveUntil src n -> ActiveUntil (Scoped None src) n

-- FIXME: Move this
data ResolveContext = ResolveToplevel | ResolveClass | ResolveInstance
    deriving ( Show, Eq )

resolveDecl :: ResolveContext -> Resolve Decl
resolveDecl rContext decl =
    case decl of
        DataDecl src isNewtype ctx dhead cons derive ->
            limitTyVarScope $
            DataDecl (Scoped None src)
                <$> resolveDataOrNew isNewtype
                <*> resolveMaybe resolveContext ctx
                <*> resolveDeclHead dhead
                <*> mapM resolveQualConDecl cons
                <*> resolveMaybe resolveDeriving derive

        FunBind src matches -> do
            -- We use the bind site to uniquely identify
            -- top-level functions. For class declarations,
            -- we use their type signature.
            when (rContext == ResolveToplevel) $
                void $ defineVariable (matchName $ head matches)
            FunBind (Scoped None src)
                <$> mapM resolveMatch matches

        -- FIXME: PatBind in classes and instances
        PatBind src pat ty rhs binds ->
            PatBind (Scoped None src)
                <$> resolvePat pat
                <*> resolveMaybe resolveType ty
                <*> resolveRhs rhs
                <*> resolveMaybe undefined binds


        -- There's an implicit forall here.
        -- Gather up all the unbound type variables and bind them
        -- to the point of the signature.
        TypeSig src names ty | rContext == ResolveToplevel -> do
            withTvRoot (getPointLoc src) $
                TypeSig (Scoped None src)
                    <$> mapM (resolveName NsVariables) names
                    <*> resolveType ty
        TypeSig src names ty | rContext == ResolveClass -> do
            withTvRoot (getPointLoc src) $
                TypeSig (Scoped None src)
                    <$> mapM defineVariable names
                    <*> resolveType ty
        ClassDecl src ctx dhead deps decls ->
            ClassDecl (Scoped None src)
                <$> resolveMaybe resolveContext ctx
                <*> resolveDeclHead dhead
                <*> mapM resolveFunDep deps
                <*> resolveMaybe (mapM resolveClassDecl) decls

        InstDecl src ctx instHead decls ->
            InstDecl (Scoped None src)
                <$> resolveMaybe resolveContext ctx
                <*> resolveInstHead instHead
                <*> resolveMaybe (mapM resolveInstDecl) decls

        ForImp src conv safety ident name ty ->
            -- Bind free type variables to the foreign import.
            withTvRoot (getPointLoc src) $
                ForImp (Scoped None src)
                    <$> resolveCallConv conv
                    <*> resolveMaybe resolveSafety safety
                    <*> pure ident
                    <*> defineVariable name
                    <*> resolveType ty

        InlineSig src noInline mbActivation qname ->
            InlineSig (Scoped None src) noInline
                <$> resolveMaybe resolveActivation mbActivation
                <*> resolveQName NsVariables qname

        TypeDecl src dhead ty ->
            TypeDecl (Scoped None src)
                <$> resolveDeclHead dhead
                <*> resolveType ty

        _ -> error $ "resolveDecl: " ++ show decl

resolveModuleName :: Resolve ModuleName
resolveModuleName (ModuleName span name) = do
    tell mempty{ scopeModuleName = name }
    pure $ ModuleName (Scoped None span) name

resolveModuleHead :: Resolve ModuleHead
resolveModuleHead (ModuleHead span name mbWarn mbExport) =
    ModuleHead (Scoped None span)
        <$> resolveModuleName name
        <*> resolveMaybe undefined mbWarn
        <*> resolveMaybe undefined mbExport

resolveModulePragma :: Resolve ModulePragma
resolveModulePragma pragma =
    case pragma of
        LanguagePragma src names ->
            pure $ LanguagePragma (Scoped None src) (map scope names)
        _ -> error "resolveModulePragma"
  where
    scope (Ident src ident) = Ident (Scoped None src) ident
    scope (Symbol src symbol) = Symbol (Scoped None src) symbol

resolveModule :: Resolve Module
resolveModule m =
    case m of
        Module span mhead pragma imports decls ->
            Module (Scoped None span)
                <$> resolveMaybe resolveModuleHead mhead
                <*> mapM resolveModulePragma pragma
                <*> mapM undefined imports
                <*> mapM (resolveDecl ResolveToplevel) decls
        _ -> error "resolveModule"


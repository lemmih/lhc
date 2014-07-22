{-
Goals:
  Intuitive interface.
  Good error messages, or at least the foundation for good error messages.
  Origin analysis of recursive modules.
  Fast.
  Support rebindable syntax.
  Support scoped type variables? Hm, maybe.

Bugs:
  Instance methods aren't resolved properly.

Notes:
  Rebindable syntax? We do not do any desugaring. Instead we just resolve
  the relevant functions and add them to the module interface. When desugaring
  with RebindableSyntax enabled, those functions can be used instead of the
  default Prelude ones.

-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Scope
    ( Origin(..)
    , NameInfo(..)
    , GlobalName(..)
    , QualifiedName(..)
    , Interface(..)
    , Source(..)
    , ScopeError(..)
    , resolve

    -- XXX: Dont export.
    , getNameIdentifier
    ) where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe
import           Language.Haskell.Exts.Annotated.Syntax
import           Language.Haskell.Exts.SrcLoc

-- Resolve all names in a module
resolve :: Module SrcSpanInfo -> ([ScopeError], Module Origin)
resolve m = runRename $ resolveModule m




-----------------------------------------------------------
-- Types and Monad

data Origin = Origin NameInfo SrcSpanInfo
    deriving ( Show )

data NameInfo
    = Resolved GlobalName
    | None
    | ScopeError ScopeError
    deriving ( Show )

data GlobalName = GlobalName SrcSpanInfo QualifiedName
    deriving ( Show, Eq, Ord )

-- Module interface, list of names in the two namespaces.
data Interface = Interface [GlobalName] [GlobalName]

-- Used for error reporting.
data Source
    = ImplicitSource -- Imported implicitly, usually from Prelude.
    | LocalSource -- Not imported, defined locally.
    | ModuleSource (ModuleName SrcSpanInfo)
    deriving ( Show )
data ScopedName = ScopedName Source GlobalName
    deriving ( Show )

data ScopeError
    = ENotInScope QualifiedName Namespace SrcSpanInfo
    -- | ETypeNotInScope QualifiedName SrcSpanInfo
    -- | EConstructorNotInScope QualifiedName SrcSpanInfo
    -- | ETypeVariableNotInScope QualifiedName SrcSpanInfo
    | EAmbiguous
    | ETypeAsClass
    | ENotExported
    | EModNotFound
    | EInternal
    deriving ( Show )
data QualifiedName = QualifiedName
    { gnameModule     :: String
    , gnameIdentifier :: String }
    deriving ( Eq, Ord, Show )
data Namespace
    = NsTypes
    | NsTypeVariables
    | NsValues
    deriving ( Show )
data Scope = Scope
    { scopeTvRoot       :: Maybe SrcSpanInfo
    , scopeModuleName   :: String
    , scopeTypes        :: Map QualifiedName [ScopedName]
    -- XXX: limitTyVarScope requires tyvars to be separate from types.
    --      Sigh.
    , scopeTyVars       :: Map QualifiedName [ScopedName]
    , scopeValues       :: Map QualifiedName [ScopedName]
    , scopeErrors       :: [ScopeError]
    }
instance Monoid Scope where
    mempty = Scope
        { scopeTvRoot       = Nothing
        , scopeModuleName   = "Main"
        , scopeTypes        = Map.empty
        , scopeTyVars       = Map.empty
        , scopeValues       = Map.empty
        , scopeErrors       = [] }
    mappend a b = Scope
        { scopeTvRoot     = scopeTvRoot a
        , scopeModuleName = scopeModuleName a
        , scopeTypes      = scopeTypes a `Map.union` scopeTypes b
        , scopeTyVars     = scopeTyVars a `Map.union` scopeTyVars b
        , scopeValues     = scopeValues a `Map.union` scopeValues b
        , scopeErrors     = scopeErrors a ++ scopeErrors b }
newtype Rename a = Rename { unRename :: ReaderT Scope (Writer Scope) a }
    deriving
        ( Monad, MonadReader Scope, MonadWriter Scope
        , Functor, Applicative )

data ResolveContext = ResolveToplevel | ResolveClass | ResolveInstance
    deriving ( Show, Eq )

type Resolve a = a SrcSpanInfo -> Rename (a Origin)

-----------------------------------------------------------
-- Utilities

runRename :: Rename a -> ([ScopeError], a)
runRename action = (scopeErrors scope, a)
  where
    (a, scope) = runWriter (runReaderT (unRename action) scope)

getTvRoot :: Rename (Maybe SrcSpanInfo)
getTvRoot = asks scopeTvRoot

withTvRoot :: SrcSpanInfo -> Rename a -> Rename a
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

getNameIdentifier :: Name l -> String
getNameIdentifier (Ident _ ident) = ident
getNameIdentifier (Symbol _ symbol) = symbol

matchName :: Match l -> Name l
matchName (Match _span name _pats _rhs _binds) = name
matchName (InfixMatch _span _left name _right _rhs _binds) = name


-----------------------------------------------------------
-- Name binding and resolution

-- FIXME: Use ETypeNotInScope, EConstructorNotInScope, etc.
resolveName :: Namespace -> Resolve Name
resolveName = resolveName' ""

resolveName' :: String -> Namespace -> Resolve Name
resolveName' = resolveName'' Nothing

resolveName'' :: Maybe SrcSpanInfo -> String -> Namespace -> Resolve Name
resolveName'' mbDefault qualification ns name =
    con <$> getScoped <*> pure nameString
  where
    (con, src, nameString) =
        case name of
            Ident a b  -> (Ident, a, b)
            Symbol a b -> (Symbol, a, b)
    qname = QualifiedName qualification nameString
    worker field = do
        m <- asks field
        let ret = case Map.lookup qname m of
                Nothing ->
                    case mbDefault of
                        Nothing ->
                            Left (ENotInScope qname ns src)
                        Just defaultValue ->
                            Right [ScopedName LocalSource (GlobalName defaultValue qname)]
                Just var -> Right var
            nameInfo =
                case ret of
                    Left err -> ScopeError err
                    Right [ScopedName _ gname] -> Resolved gname
                    _ -> error "resolveName: ambiguous"
        tell mempty{ scopeErrors =
                        maybeToList (either (Just) (const Nothing) ret) }
        return $ Origin nameInfo src
    getScoped =
        case ns of
            NsValues        -> worker scopeValues
            NsTypes         -> worker scopeTypes
            NsTypeVariables -> worker scopeTyVars

resolveTyVar :: Resolve Name
resolveTyVar name = do
    mbDefault <- getTvRoot
    resolveName'' mbDefault "" NsTypeVariables name

resolveQName :: Namespace -> Resolve QName
resolveQName ns qname =
    case qname of
        Qual src (ModuleName l m) name ->
            Qual (Origin None src)
                <$> pure (ModuleName (Origin None l) m)
                <*> resolveName' m ns name
        UnQual src name -> do
            name' <- resolveName ns name
            let Origin origin _ = ann name'
            UnQual (Origin origin src)
                <$> pure name'
        Special src specialCon ->
            Special (Origin None src)
                <$> resolveSpecialCon specialCon

defineName :: Namespace -> Resolve Name
defineName ns name = do
    thisModule <- asks scopeModuleName
    let src = ann name
        ident = getNameIdentifier name
        qname = QualifiedName "" ident
        gname = GlobalName src (QualifiedName thisModule ident)
        resolved = ScopedName LocalSource gname
    case ns of
        NsValues ->
            tell mempty{ scopeValues = Map.singleton qname [resolved]}
        NsTypes  ->
            tell mempty{ scopeTypes = Map.singleton qname [resolved]}
        NsTypeVariables ->
            tell mempty{ scopeTyVars = Map.singleton qname [resolved] }
    resolveName ns name

-- resolveMaybe :: Resolve a -> Resolve (Maybe a)
resolveMaybe :: (a -> Rename b) -> Maybe a -> Rename (Maybe b)
resolveMaybe fn mbValue =
    case mbValue of
        Nothing    -> return Nothing
        Just value -> Just <$> fn value

-------------------------------------------------------------
---- Name resolution

resolveSafety :: Resolve Safety
resolveSafety = pure . fmap (Origin None)

resolveCallConv :: Resolve CallConv
resolveCallConv = pure . fmap (Origin None)

resolveKind :: Resolve Kind
resolveKind kind =
    case kind of
        KindStar src -> pure $ KindStar (Origin None src)
        _ -> error "resolveKind"

resolveTyVarBind :: Resolve TyVarBind
resolveTyVarBind tyVarBind =
    case tyVarBind of
        KindedVar src name kind ->
            KindedVar
                <$> pure (Origin None src)
                <*> defineName NsTypeVariables name
                <*> resolveKind kind
        UnkindedVar src name    ->
            UnkindedVar
                <$> pure (Origin None src)
                <*> defineName NsTypeVariables name

resolveDeclHead :: Resolve DeclHead
resolveDeclHead dhead =
    case dhead of
        DHead src name tyVarBinds ->
            DHead
                <$> pure (Origin None src)
                <*> defineName NsTypes name
                <*> mapM resolveTyVarBind tyVarBinds
        DHInfix{} -> error "resolveDeclHead"
        DHParen _ next -> resolveDeclHead next

resolveContext :: Resolve Context
resolveContext ctx =
    case ctx of
        _ -> error "resolveContext"

resolveDataOrNew :: Resolve DataOrNew
resolveDataOrNew = pure . fmap (Origin None)

resolveInstHead :: Resolve InstHead
resolveInstHead instHead =
    case instHead of
        IHead src className tys ->
            IHead (Origin None src)
                <$> resolveQName NsTypes className
                <*> mapM resolveType tys
        IHInfix{} -> error "resolveInstHead"
        IHParen src sub ->
            IHParen (Origin None src)
                <$> resolveInstHead sub

resolveDeriving :: Resolve Deriving
resolveDeriving (Deriving src instHeads) =
    Deriving
        <$> pure (Origin None src)
        <*> mapM resolveInstHead instHeads

resolveSpecialCon :: Resolve SpecialCon
resolveSpecialCon specialCon = pure $
    case specialCon of
        UnitCon src             -> UnitCon $ Origin None src
        ListCon src             -> ListCon $ Origin None src
        FunCon src              -> FunCon $ Origin None src
        TupleCon src boxed size -> TupleCon (Origin None src) boxed size
        Cons src                -> Cons $ Origin None src
        UnboxedSingleCon src    -> UnboxedSingleCon $ Origin None src

resolveType :: Resolve Type
resolveType ty =
    case ty of
        TyCon src qname ->
            TyCon (Origin None src)
                <$> resolveQName NsTypes qname
        TyVar src tyVar ->
            TyVar (Origin None src)
                <$> resolveTyVar tyVar
        TyFun src a b ->
            TyFun (Origin None src)
                <$> resolveType a
                <*> resolveType b
        TyApp src a b ->
            TyApp (Origin None src)
                <$> resolveType a
                <*> resolveType b
        TyParen src sub ->
            TyParen (Origin None src)
                <$> resolveType sub
        TyTuple src boxed tys ->
            TyTuple (Origin None src) boxed
                <$> mapM resolveType tys
        _ -> error $ "resolveType: " ++ show ty

resolveBangType :: Resolve BangType
resolveBangType bangTy =
    case bangTy of
        BangedTy src ty ->
            BangedTy (Origin None src)
                <$> resolveType ty
        UnBangedTy src ty ->
            UnBangedTy (Origin None src)
                <$> resolveType ty
        UnpackedTy src ty ->
            UnpackedTy (Origin None src)
                <$> resolveType ty

resolveFieldDecl :: Resolve FieldDecl
resolveFieldDecl fieldDecl =
    case fieldDecl of
        FieldDecl src names bangTy ->
            FieldDecl (Origin None src)
                <$> mapM (defineName NsValues) names
                <*> resolveBangType bangTy

resolveConDecl :: Resolve ConDecl
resolveConDecl conDecl =
    case conDecl of
        ConDecl src name bangTys ->
            ConDecl (Origin None src)
                <$> defineName NsValues name
                <*> mapM resolveBangType bangTys
        RecDecl src name fieldDecls ->
            RecDecl (Origin None src)
                <$> defineName NsValues name
                <*> mapM resolveFieldDecl fieldDecls
        _ -> error "resolveConDecl"

resolveQualConDecl :: Resolve QualConDecl
resolveQualConDecl (QualConDecl src mbTyVarBinds ctx conDecl) =
    QualConDecl (Origin None src)
        <$> resolveMaybe (mapM resolveTyVarBind) mbTyVarBinds
        <*> resolveMaybe resolveContext ctx
        <*> resolveConDecl conDecl

resolvePat :: Resolve Pat
resolvePat pat =
    case pat of
        PVar src name ->
            PVar (Origin None src)
                <$> defineName NsValues name
        PApp src con pats ->
            PApp (Origin None src)
                <$> resolveQName NsValues con
                <*> mapM resolvePat pats
        PWildCard src ->
            pure $ PWildCard (Origin None src)
        PParen src sub ->
            PParen (Origin None src)
                <$> resolvePat sub
        PTuple src boxed pats ->
            PTuple (Origin None src) boxed
                <$> mapM resolvePat pats
        _ -> error $ "resolvePat: " ++ show pat

resolveGuardedAlts :: Resolve GuardedAlts
resolveGuardedAlts galts =
    case galts of
        UnGuardedAlt src expr ->
            UnGuardedAlt (Origin None src)
                <$> resolveExp expr
        _ -> error "resolveGuardedAlts"

resolveAlt :: Resolve Alt
resolveAlt (Alt src pat guarded mbBinds) = limitScope $
    Alt (Origin None src)
        <$> resolvePat pat
        <*> resolveGuardedAlts guarded
        <*> resolveMaybe undefined mbBinds

resolveQOp :: Resolve QOp
resolveQOp qop =
    case qop of
        QVarOp src qname ->
            QVarOp (Origin None src)
                <$> resolveQName NsValues qname
        QConOp src qname ->
            QConOp (Origin None src)
                <$> resolveQName NsValues qname

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
        con (Origin None src)

resolveExp :: Resolve Exp
resolveExp expr =
    case expr of
        Case src scrut alts ->
            Case (Origin None src)
                <$> resolveExp scrut
                <*> mapM resolveAlt alts
        Con src qname ->
            Con (Origin None src)
                <$> resolveQName NsValues qname
        Var src qname ->
            Var (Origin None src)
                <$> resolveQName NsValues qname
        App src a b ->
            App (Origin None src)
                <$> resolveExp a
                <*> resolveExp b
        InfixApp src a qop b ->
            InfixApp (Origin None src)
                <$> resolveExp a
                <*> resolveQOp qop
                <*> resolveExp b
        Paren src sub ->
            Paren (Origin None src)
                <$> resolveExp sub
        Lambda src pats sub -> limitScope $
            Lambda (Origin None src)
                <$> mapM resolvePat pats
                <*> resolveExp sub
        Lit src lit ->
            Lit (Origin None src)
                <$> resolveLiteral lit
        Tuple src boxed exps ->
            Tuple (Origin None src) boxed
                <$> mapM resolveExp exps
        _ -> error $ "resolveExp: " ++ show expr

resolveRhs :: Resolve Rhs
resolveRhs rhs =
    case rhs of
        UnGuardedRhs src expr ->
            UnGuardedRhs (Origin None src)
                <$> resolveExp expr
        _ -> error "resolveRhs"

resolveMatch :: Resolve Match
resolveMatch match =
    case match of
        Match src name pats rhs mbBinds -> limitScope $
            Match (Origin None src)
                <$> resolveName NsValues name
                <*> mapM resolvePat pats
                <*> resolveRhs rhs
                <*> resolveMaybe undefined mbBinds
        _ -> error "resolveMatch"

resolveClassDecl :: Resolve ClassDecl
resolveClassDecl decl =
    case decl of
        ClsDecl src sub ->
          ClsDecl (Origin None src)
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
            InsDecl (Origin None src)
                <$> resolveDecl ResolveInstance decl
        _ -> error "resolveInstDecl"

resolveActivation :: Resolve Activation
resolveActivation activation = pure $
    case activation of
        ActiveFrom src n -> ActiveFrom (Origin None src) n
        ActiveUntil src n -> ActiveUntil (Origin None src) n

resolveDecl :: ResolveContext -> Resolve Decl
resolveDecl rContext decl =
    case decl of
        DataDecl src isNewtype ctx dhead cons derive ->
            limitTyVarScope $
            DataDecl (Origin None src)
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
                void $ defineName NsValues (matchName $ head matches)
            FunBind (Origin None src)
                <$> mapM resolveMatch matches

        -- FIXME: PatBind in classes and instances
        PatBind src pat ty rhs binds ->
            PatBind (Origin None src)
                <$> resolvePat pat
                <*> resolveMaybe resolveType ty
                <*> resolveRhs rhs
                <*> resolveMaybe undefined binds


        -- There's an implicit forall here.
        -- Gather up all the unbound type variables and bind them
        -- to the point of the signature.
        TypeSig src names ty | rContext == ResolveToplevel -> do
            -- FIXME: Bind tyvars to the src of the definition, not the tysig.
            withTvRoot src $
                TypeSig (Origin None src)
                    <$> mapM (resolveName NsValues) names
                    <*> resolveType ty
        TypeSig src names ty | rContext == ResolveClass -> do
            withTvRoot src $
                TypeSig (Origin None src)
                    <$> mapM (defineName NsValues) names
                    <*> resolveType ty
        ClassDecl src ctx dhead deps decls ->
            ClassDecl (Origin None src)
                <$> resolveMaybe resolveContext ctx
                <*> resolveDeclHead dhead
                <*> mapM resolveFunDep deps
                <*> resolveMaybe (mapM resolveClassDecl) decls

        InstDecl src ctx instHead decls ->
            InstDecl (Origin None src)
                <$> resolveMaybe resolveContext ctx
                <*> resolveInstHead instHead
                <*> resolveMaybe (mapM resolveInstDecl) decls

        ForImp src conv safety ident name ty ->
            -- Bind free type variables to the foreign import.
            withTvRoot src $
                ForImp (Origin None src)
                    <$> resolveCallConv conv
                    <*> resolveMaybe resolveSafety safety
                    <*> pure ident
                    <*> defineName NsValues name
                    <*> resolveType ty

        InlineSig src noInline mbActivation qname ->
            InlineSig (Origin None src) noInline
                <$> resolveMaybe resolveActivation mbActivation
                <*> resolveQName NsValues qname

        TypeDecl src dhead ty ->
            TypeDecl (Origin None src)
                <$> resolveDeclHead dhead
                <*> resolveType ty

        _ -> error $ "resolveDecl: " ++ show decl

resolveModuleName :: Resolve ModuleName
resolveModuleName (ModuleName src name) = do
    tell mempty{ scopeModuleName = name }
    pure $ ModuleName (Origin None src) name

resolveExportSpec :: Resolve ExportSpec
resolveExportSpec spec =
    case spec of
        EAbs src qname ->
            EAbs (Origin None src) <$> resolveQName NsTypes qname
        _ -> error $ "resolveExportSpec: " ++ show spec

resolveExportSpecList :: Resolve ExportSpecList
resolveExportSpecList list =
    case list of
        ExportSpecList src exports ->
            ExportSpecList (Origin None src)
                <$> mapM resolveExportSpec exports

resolveModuleHead :: Resolve ModuleHead
resolveModuleHead (ModuleHead src name mbWarn mbExport) =
    ModuleHead (Origin None src)
        <$> resolveModuleName name
        <*> resolveMaybe undefined mbWarn
        <*> resolveMaybe resolveExportSpecList mbExport

resolveModulePragma :: Resolve ModulePragma
resolveModulePragma pragma =
    case pragma of
        LanguagePragma src names ->
            pure $ LanguagePragma (Origin None src) (map scope names)
        _ -> error "resolveModulePragma"
  where
    scope (Ident src ident) = Ident (Origin None src) ident
    scope (Symbol src symbol) = Symbol (Origin None src) symbol

resolveModule :: Resolve Module
resolveModule m =
    case m of
        Module src mhead pragma imports decls ->
            Module (Origin None src)
                <$> resolveMaybe resolveModuleHead mhead
                <*> mapM resolveModulePragma pragma
                <*> mapM undefined imports
                <*> mapM (resolveDecl ResolveToplevel) decls
        _ -> error "resolveModule"


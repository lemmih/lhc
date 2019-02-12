{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Language.Haskell.Crux.FromHaskell
    ( convert
    ) where

import           Control.Monad.Reader
import           Control.Monad.RWS          (RWS, execRWS)
import           Control.Monad.State
import           Control.Monad.Writer       (MonadWriter (..))
import           Data.List                  (transpose)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Semigroup             (Semigroup (..))
import           Data.Typeable
import qualified Language.Haskell.Exts      as HS

import           Language.Haskell.Crux
import           Language.Haskell.Scope     (Entity (..), NameInfo (..),
                                             QualifiedName (..))
import qualified Language.Haskell.Scope     as Scope
import           Language.Haskell.TypeCheck (Qualified (..), TcEnv (..),
                                             TcVar (..), Typed)
import qualified Language.Haskell.TypeCheck as TC
import qualified LLVM.AST                   as LLVM
import qualified LLVM.AST.AddrSpace         as LLVM

data Scope = Scope
    { scopeTcEnv    :: TcEnv
    , scopeNewTypes :: [Variable]
    }

data Env = Env
    { envForeigns     :: [Foreign]
    , envNodes        :: [NodeDefinition]
    , envNewTypes     :: [NewType]
    , envDecls        :: [Declaration]
    , envConstructors :: Map Name Name
    }

instance Semigroup Env where
  a<>b = Env
      { envForeigns = w envForeigns
      , envNodes    = w envNodes
      , envNewTypes = w envNewTypes
      , envDecls    = w envDecls
      , envConstructors = w envConstructors
      }
    where w f = mappend (f a) (f b)
instance Monoid Env where
    mempty = Env
        { envForeigns = mempty
        , envNodes    = mempty
        , envNewTypes = mempty
        , envDecls    = mempty
        , envConstructors = mempty
        }
    mappend = (<>)

newtype M a = M { unM :: RWS Scope Env Int a }
    deriving
        ( Monad, Functor, Applicative
        , MonadReader Scope, MonadState Int
        , MonadWriter Env )

runM :: TcEnv -> M a -> (Int, Env)
runM tcEnv m = (ns', env)
  where
    (ns', env) = execRWS (unM m) (Scope tcEnv []) ns
    ns = 0

pushForeign :: Foreign -> M ()
pushForeign f = tell mempty{ envForeigns = [f] }

pushDecl :: Declaration -> M ()
pushDecl decl = tell mempty{ envDecls = [decl] }

pushNode :: NodeDefinition -> M ()
pushNode def = tell mempty{ envNodes = [def] }

pushNewType :: NewType -> M ()
pushNewType def = tell mempty{ envNewTypes = [def] }

newUnique :: M Int
newUnique = do
    ns <- get
    put (ns+1)
    return ns

newName :: String -> M Name
newName ident = do
    u <- newUnique
    return $ Name [] ident u

-- newNameFrom :: Name -> M Name
-- newNameFrom (Name ms ident _) = do
--   u <- newUnique
--   return $ Name ms ident u
--
-- newVariableFrom :: Variable -> M Variable
-- newVariableFrom v = do
--   name' <- newNameFrom (varName v)
--   return $ v{varName = name'}

nameInfo :: HS.Annotated ast => ast Typed -> Scope.NameInfo
nameInfo ast =
  case HS.ann ast of
    TC.Coerced info _src _proof -> info
    TC.Scoped info _src         -> info

nameFromEntity :: Entity -> Name
nameFromEntity entity =
  case entityName entity of
    QualifiedName m ident -> Name [m] ident 0

expectEntity :: HS.Name Typed -> Entity
expectEntity hsName =
  case nameInfo hsName of
    Scope.Resolved entity -> entity
    Scope.Binding entity  -> entity
    Scope.None            -> error "expectEntity: None"
    Scope.ScopeError err  -> error $ "expectEntity: ScopeError " ++ show err

bindName :: HS.Name Typed -> Name
bindName = nameFromEntity . expectEntity

bindVariable :: HS.Name Typed -> M Variable
bindVariable hsName = do
    let name = bindName hsName
    ty <- lookupType hsName
    return $ Variable name ty

lookupType :: HS.Name Typed -> M TC.Type
lookupType hsName = do
  case nameInfo hsName of
    Resolved entity -> do
      tcEnv <- asks scopeTcEnv
      case Map.lookup entity (tcEnvValues tcEnv) of
        Nothing -> error "Missing type info"
        Just ty -> return ty
    Scope.Binding entity -> do
      tcEnv <- asks scopeTcEnv
      case Map.lookup entity (tcEnvValues tcEnv) of
        Nothing -> error "Missing type info"
        Just ty -> return ty
    _ -> error "Urk"

bindConstructor :: HS.Name Typed -> Name
bindConstructor dataCon =
  case nameInfo dataCon of
    Scope.Binding entity ->
      let (QualifiedName m ident) = entityName entity
      in Name [m] ident 0
    Scope.Resolved _ -> error "bindConstructor: Resolved"
    Scope.None -> error "bindConstructor: None"
    Scope.ScopeError err -> error $ "bindConstructor: ScopeError " ++ show err

resolveName :: HS.Name Typed -> M Name
resolveName hsName =
    case nameInfo hsName of
        Scope.Resolved entity -> do
            let QualifiedName m ident = entityName entity
            let name = Name [m] ident 0
            return name
        -- Resolved gname -> do
        --     asks $ Map.findWithDefault scopeError gname . scopeVariables
        --Scope.Global gname ->
        --    asks $ Map.findWithDefault scopeError gname . scopeConstructors
        _ -> scopeError
  where
    scopeError = error $ "resolveName: Not in scope: " ++
                    getNameIdentifier hsName

-- resolveQualifiedName :: QualifiedName -> M Name
-- resolveQualifiedName qname =
--     asks $ Map.findWithDefault scopeError qname . scopeNodes
--   where
--     scopeError = error $ "resolveGlobalName: Not in scope: " ++ show qname

resolveQName :: HS.QName Typed -> M Variable
resolveQName qname =
    case qname of
        HS.Qual _ _ name          -> do
          n <- resolveName name
          ty <- lookupType name
          return $ Variable n ty
        HS.UnQual _ name          -> do
          n <- resolveName name
          ty <- lookupType name
          return $ Variable n ty
        HS.Special _ HS.UnitCon{} -> return unitCon
        HS.Special _ HS.Cons{}    -> return consCon
        -- HS.Special _ HS.ListCon{} -> return nilCon
        _ -> unhandledSyntax qname

convert :: TcEnv -> HS.Module Typed -> Module
convert tcEnv (HS.Module _ _ _ _ decls) = Module
    { cruxForeigns  = envForeigns env
    , cruxDecls     = envDecls env
    , cruxNodes     = envNodes env
    , cruxNewTypes  = envNewTypes env }
  where
    (_ns, env) = runM tcEnv $ do
        nts <- catMaybes <$> mapM convertNewTypes decls
        local (\s -> s{scopeNewTypes = nts}) $
          mapM_ convertDecl decls
convert _ m = unhandledSyntax m

-- Return function name.
matchInfo :: [HS.Match Typed] -> HS.Name Typed
matchInfo [] =
    error "Language.Haskell.Crux.FromHaskell.matchInfo"
matchInfo (HS.Match _ name _pats _ _:_) = name
matchInfo (HS.InfixMatch _ _ name _pats _rhs _:_) = name

{-
Sometimes we have introduce new arguments:
fn (Just val) = ...
=>
fn arg = case arg of Just val -> ...

In the above case we cannot find a good name but in many cases we can do
better. Consider:
fn x@(Just val) = ...
=>
fn x = case x of Just val -> ...

fn [] = ...
fn lst = ...
=>
fn lst = case lst of [] -> ...; _ -> ...

matchArgNames uses heuristics to figure out which user variable names can be
reused.
-}
matchArgNames :: [HS.Match Typed] -> [Maybe (HS.Name Typed)]
matchArgNames = map collapse . transpose . map worker
  where
    collapse = listToMaybe . catMaybes
    worker (HS.Match _ _ pats _ _)          = map fromPat pats
    worker (HS.InfixMatch _ pat _ pats _ _) = map fromPat (pat:pats)
    fromPat (HS.PVar _ name)     = Just name
    fromPat (HS.PAsPat _ name _) = Just name
    fromPat (HS.PParen _ pat)    = fromPat pat
    fromPat _                    = Nothing

convertDecl :: HS.Decl Typed -> M ()
convertDecl decl =
    mapM_ pushDecl =<< convertDecl' decl

reifyTypeVariables :: TC.Type -> Expr -> Expr
reifyTypeVariables (TC.TyForall tvs _) = Lam (map tcVarToVariable tvs)
reifyTypeVariables _                   = id

convertNewTypes :: HS.Decl Typed -> M (Maybe Variable)
convertNewTypes decl =
  case decl of
    HS.DataDecl _ HS.NewType{} _ctx dhead [HS.QualConDecl _ _tyvars _ (HS.ConDecl _ name _)] _deriving -> do
      dheadToNode dhead
      ty <- lookupType name
      let nt = Variable (bindConstructor name) ty
      pushNewType $ IsNewType nt
      return $ Just nt
    _ -> return Nothing

convertDecl' :: HS.Decl Typed -> M [Declaration]
convertDecl' decl =
  case decl of
    HS.FunBind tyDecl matches -> do
      let proof =
            case tyDecl of
              TC.Coerced _ _ p -> p
              TC.Scoped{}      -> error "missing proof"
      let name = matchInfo matches
          fnArgNames = matchArgNames matches
      let ty = TC.reifyProof proof
      argNames <- forM fnArgNames $ maybe (newName "arg") (pure.bindName)
      let args = zipWith Variable argNames (splitTy ty)
      decl' <- Declaration
          <$> pure ty
          <*> pure (bindName name)
          <*> (reifyTypeVariables ty <$> Lam args
                  <$> convertMatches args matches)
      return [decl']
    HS.PatBind tyDecl (HS.PVar _ name) rhs binds -> do
      let proof =
            case tyDecl of
              TC.Coerced _ _ p -> p
              TC.Scoped{}      -> error "missing proof"
      let ty = TC.reifyProof proof
      decl' <- Declaration
          <$> lookupType name
          <*> pure (bindName name)
          <*> (reifyTypeVariables ty <$> (convertBinds binds =<< convertRhs rhs))
      return [decl']
    HS.ForImp _ _conv _safety mbExternal name _ty -> do
      let external = fromMaybe (getNameIdentifier name) mbExternal
      foreignTy <- lookupType name
      decl' <- Declaration
          <$> lookupType name
          <*> pure (bindName name)
          <*> convertExternal external foreignTy

      unless (isPrimitive external) $ do
          let (argTypes, _isIO, retType) = ffiTypes foreignTy
          pushForeign Foreign
              { foreignName = external
              , foreignReturn = toCType retType
              , foreignArguments = map toCType argTypes }

      return [decl']
    HS.DataDecl _ HS.DataType{} _ctx dhead qualCons _deriving -> do
      dheadToNode dhead
      mapM_ (convertQualCon False) qualCons
      return []
    HS.DataDecl _ HS.NewType{} _ctx _dhead _qualCons _deriving -> return []
    HS.TypeSig{} -> return []
    HS.InlineSig{} -> return []
    _ -> unhandledSyntax decl

isPrimitive :: String -> Bool
isPrimitive "realworld#" = True
isPrimitive "cast"       = True
isPrimitive "-#"         = True
isPrimitive "sdiv#"      = True
isPrimitive "srem#"      = True
isPrimitive _            = False

annotateName :: String -> Name -> Name
annotateName ns (Name m ident uniq) = Name (ns:m) ident uniq

dheadToNode :: HS.DeclHead Typed -> M ()
dheadToNode dhead = do
    let (hsName, args) = split dhead []
    let name = annotateName "@" $ nameFromEntity $ expectEntity hsName
    pushNode $ NodeDefinition (Variable name (foldr TC.TyFun TC.TyStar args))
  where
    split (HS.DHead _ name) acc       = (name, reverse acc)
    split (HS.DHApp _ dh _ty) acc     = split dh (TC.TyStar : acc)
    split (HS.DHInfix _ _ty name) acc = (name, reverse (TC.TyStar : acc))
    split (HS.DHParen _ dh) acc       = split dh acc

convertMatches :: [Variable] -> [HS.Match Typed] -> M Expr
convertMatches _args [] = error "Language.Haskell.Crux.FromHaskell.convertMatches"
convertMatches args [HS.InfixMatch _ pat _ pats rhs mbBinds] =
    convertBinds mbBinds =<< convertAltPats (zip args (pat:pats)) Nothing =<< convertRhs rhs
convertMatches args [HS.Match _ _ pats rhs mbBinds] =
    convertBinds mbBinds =<< convertAltPats (zip args pats) Nothing =<< convertRhs rhs
convertMatches args (HS.Match _ _ pats rhs mbBinds:xs)
    | all isSimplePat pats = do
        rest <- convertMatches args xs
        convertBinds mbBinds =<<
          convertAltPats (zip args pats) (Just rest) =<<
                convertRhs rhs
    | otherwise = do
        rest <- convertMatches args xs
        restBranch <- Variable <$> newName "branch" <*> exprType rest
        e <- convertAltPats (zip args pats) (Just $ Var restBranch) =<<
                convertRhs rhs
        convertBinds mbBinds $ Let (NonRec restBranch rest) e
convertMatches _args (match:_) = unhandledSyntax match

convertBinds :: Maybe (HS.Binds Typed) -> Expr -> M Expr
convertBinds Nothing e = pure e
convertBinds (Just (HS.BDecls _ binds)) e = do
  decls <- mapM convertDecl' binds
  pure $ Let (Rec [ (Variable name ty, body)
           | Declaration ty name body <- concat decls ])
         e
convertBinds _ _ = error "Invalid bind"

convertAltPats :: [(Variable, HS.Pat Typed)] -> Maybe Expr -> Expr -> M Expr
convertAltPats conds failBranch successBranch =
    case conds of
        [] -> pure successBranch
        ((scrut,pat) : more)
            | isSimplePat pat ->
                convertAltPat (Var scrut) failBranch pat =<<
                    convertAltPats more failBranch successBranch
            | otherwise -> do
                rest <- convertAltPats more failBranch successBranch
                restBranch <- Variable <$> newName "branch" <*> exprType rest
                e <- convertAltPat (Var scrut) failBranch pat (Var restBranch)
                return $ Let (NonRec restBranch rest) e


-- XXX: Don't use Bool for isNewtype
convertQualCon :: Bool -> HS.QualConDecl Typed -> M ()
convertQualCon isNewtype (HS.QualConDecl _ _tyvars _ctx con) =
    convertConDecl isNewtype con

-- XXX: Don't use Bool for isNewtype
convertConDecl :: Bool -> HS.ConDecl Typed -> M ()
convertConDecl isNewtype con =
  case con of
    HS.ConDecl _ name _tys -> do
      let conName = bindConstructor name
      ty <- lookupType name
      if isNewtype
        then pushNewType $ IsNewType $ Variable conName ty
        else pushNode $ NodeDefinition (Variable conName ty) -- (init $ splitTy ty)
    --HS.RecDecl _ name fieldDecls -> do
    _ -> unhandledSyntax con

-- XXX: Temporary measure. 2014-07-11
splitTy :: TC.Type -> [TC.Type]
splitTy (TC.TyForall _ (_ :=> ty)) = splitTy ty
splitTy (TC.TyFun a b)             = a : splitTy b
splitTy ty                         = [ty]

toCType :: TC.Type -> LLVM.Type
toCType ty =
    case ty of
        TC.TyApp (TC.TyCon qname) ty'
            | qname == QualifiedName "LHC.Prim" "Addr" ->
                LLVM.PointerType (toCType ty') (LLVM.AddrSpace 0)
            | qname == QualifiedName "LHC.Prim" "Ptr" ->
                LLVM.PointerType (toCType ty') (LLVM.AddrSpace 0)
        TC.TyCon qname
            | qname == QualifiedName "LHC.Prim" "I8" ->
                LLVM.IntegerType 8
            | qname == QualifiedName "LHC.Prim" "I32" ->
                LLVM.IntegerType 32
            | qname == QualifiedName "LHC.Prim" "Int32" ->
                LLVM.IntegerType 32
            | qname == QualifiedName "LHC.Prim" "I64" ->
                LLVM.IntegerType 64
            | qname == QualifiedName "LHC.Prim" "Unit" ->
                LLVM.VoidType
        TC.TyApp (TC.TyCon qname) ty'
            | qname == QualifiedName "LHC.Prim" "IO" ->
                toCType ty'
        TC.TyCon qname
            | qname == QualifiedName "LHC.Prim" "RealWorld#" ->
                LLVM.NamedTypeReference (LLVM.mkName "word")
        _ -> error $ "toCType: " ++ show ty

-- convertBangType :: HS.BangType Origin -> M Type
-- convertBangType bty =
--     case bty of
--         HS.UnBangedTy _ ty -> convertType ty
--         HS.BangedTy _ ty -> convertType ty
--         _ -> error "convertBangType"


-- cfun :: Addr I8 -> IO ()
-- \ptr -> IO (\s -> WithExternal cfun Void [ptr,s]) (IOUnit boxed s))
-- cfun :: Addr I8 -> IO CInt
-- \ptr -> IO (\s -> WithExternal cfun CInt [ptr,s]) (IOUnit boxed s))
-- cfun :: CInt -> CInt
-- \cint -> WithExternal cfun [cint] boxed
convertExternal :: String -> TC.Type -> M Expr
convertExternal "realworld#" _ty = return (Lit LitVoid)
convertExternal "cast" ty = do
    arg <- Variable <$> newName "arg" <*> pure argType
    return $ Lam [arg] $ Convert (Var arg) retType
  where
    ([argType], _isIO, retType) = ffiTypes ty
convertExternal cName ty
    | isIO = do
        args <- forM argTypes $ \t -> Variable <$> newName "arg" <*> pure t
        primOut <- Variable <$> newName "primOut" <*> pure primRetType
        s <- Variable
                <$> newName "s"
                <*> pure realWorld
        s' <- Variable
                <$> newName "s'"
                <*> pure realWorld
        -- boxed <- Variable <$> newName "boxed" <*> pure retType

        return $
            Lam args $
            let action = Lam [s] $
                    WithExternal primOut s' cName (map Var args) (Var s) $
                    UnboxedTuple [Var s', boxRetType (Var primOut)]
            in action
    | otherwise = do -- not isIO
        args <- forM argTypes $ \t -> Variable <$> newName "arg" <*> pure t
        primOut <- Variable <$> newName "primOut" <*> pure retType
        return $
            Lam args $
            ExternalPure primOut cName (map Var args) $
            Var primOut
  where
    (argTypes, isIO, retType) = ffiTypes ty
    (primRetType, boxRetType) = marshalType retType

marshalType :: TC.Type -> (TC.Type, Expr -> Expr)
marshalType ty =
  case ty of
    TC.TyCon (QualifiedName "LHC.Prim" "Int32") ->
      (i32, App (Con int32Con))
    TC.TyApp (TC.TyCon (QualifiedName "LHC.Prim" "Ptr"))
             (TC.TyCon (QualifiedName "LHC.Prim" "I8"))->
      (TC.TyApp addrTy i8, App (Con ptrCon))
    _ -> error $ "Can't marshal: " ++ show ty

ffiTypes :: TC.Type -> ([TC.Type], Bool, TC.Type)
ffiTypes = worker []
  where
    worker acc ty =
        case ty of
            TC.TyFun t ty' -> worker (t : acc) ty'
            TC.TyApp (TC.TyCon qname) sub
                | qname == QualifiedName "LHC.Prim" "IO"
                    -> (reverse acc, True, sub)
            _ -> (reverse acc, False, ty)
            --_ -> error "ffiArguments"

convertRhs :: HS.Rhs Typed -> M Expr
convertRhs rhs =
    case rhs of
        HS.UnGuardedRhs _ expr -> convertExp expr
        _                      -> unhandledSyntax rhs

convertStmts :: [HS.Stmt Typed] -> M Expr
convertStmts [] = error "convertStmts: Empty list"
convertStmts [end] =
    case end of
        -- HS.Generator _ pat expr
        HS.Qualifier _ expr -> convertExp expr
        _                   -> unhandledSyntax end
convertStmts (x:xs) =
  case x of
    HS.Generator (TC.Coerced _ _ proof) (HS.PVar _ name) expr -> do
      var <- bindVariable name
      expr' <- convertExp expr
      rest <- convertStmts xs
      return $ convertProof proof primBindIO `App` expr' `App` Lam [var] rest
    HS.Qualifier (TC.Coerced _ _ proof) expr -> do
      expr' <- convertExp expr
      rest <- convertStmts xs
      return $ convertProof proof primThenIO `App` expr' `App` rest
    _ -> unhandledSyntax x

primThenIO :: Expr
primThenIO = Var (Variable name ty)
  where
    name = Name ["LHC.Prim"] "thenIO" 0
    ty = TC.TyForall [aRef, bRef] ([] :=> (ioA `TC.TyFun` ioB `TC.TyFun` ioB))
    aRef = TcVar "a" []
    bRef = TcVar "b" []
    ioA = io `TC.TyApp` TC.TyRef aRef
    ioB = io `TC.TyApp` TC.TyRef bRef

primBindIO :: Expr
primBindIO = Var (Variable name ty)
  where
    name = Name ["LHC.Prim"] "bindIO" 0
    ty = TC.TyForall [aRef, bRef] ([] :=> (ioA `TC.TyFun` ioAB `TC.TyFun` ioB))
    aRef = TcVar "a" []
    bRef = TcVar "b" []
    ioA = io `TC.TyApp` TC.TyRef aRef
    ioB = io `TC.TyApp` TC.TyRef bRef
    ioAB = TC.TyRef aRef `TC.TyFun` ioB

unpackString :: Expr
unpackString = Var (Variable name ty)
  where
    name = Name ["LHC.Prim"] "unpackString#" 0
    ty = TC.TyApp addr i8 `TC.TyFun` TC.TyList char
    addr = TC.TyCon (QualifiedName "LHC.Prim" "Addr")
    i8 = TC.TyCon (QualifiedName "LHC.Prim" "I8")
    char = TC.TyCon (QualifiedName "LHC.Prim" "Char")


findProof :: HS.QName Typed -> Expr -> Expr
-- findProof _ = id
findProof name =
    case tyDecl of
      TC.Coerced _ _ proof -> convertProof proof
      TC.Scoped{}          -> id
  where
    tyDecl =
      case name of
        HS.UnQual _ qname -> HS.ann qname
        _                 -> HS.ann name

tcVarToVariable :: TC.TcVar -> Variable
tcVarToVariable (TC.TcVar name _loc) =
  Variable (Name ["@"] name 0) TC.TyStar

convertProof :: TC.Proof -> Expr -> Expr
convertProof (TC.ProofAbs tvs p) = Lam (map tcVarToVariable tvs) . convertProof p
convertProof TC.ProofSrc{} = id
convertProof (TC.ProofAp p args) =
  \e -> foldl App (convertProof p e) (map tyToExpr args)
convertProof p = error $ "Weird proof: " ++ show p

tyToExpr :: TC.Type -> Expr
tyToExpr (TC.TyRef ref) = Var (tcVarToVariable ref)
tyToExpr (TC.TyList elt) = App (Con listCon) (tyToExpr elt)
tyToExpr (TC.TyCon (QualifiedName m ident)) = Con (Variable (Name ["@",m] ident 0) TC.TyStar)
tyToExpr (TC.TyTuple []) = Con unitTCon
tyToExpr (TC.TyApp a b) = App (tyToExpr a) (tyToExpr b)
tyToExpr ty             = error $ "Weird type: " ++ show ty


convertExp :: HS.Exp Typed -> M Expr
convertExp expr =
  case expr of
    HS.Var _ name -> do
      var <- resolveQName name
      return $ findProof name (Var var)
    HS.Con _ name -> do
      var <- resolveQName name
      nts <- asks scopeNewTypes
      if var `elem` nts
        then return Cast
        else return $ Con var
    HS.App _ a b ->
      App
        <$> convertExp a
        <*> convertExp b
    HS.InfixApp _ a (HS.QConOp _ con) b -> do
      ae <- convertExp a
      be <- convertExp b
      var <- resolveQName con
      -- pure $ App (App (findProof con (Con var)) ae) be
      pure $ App (App (Con var) ae) be
    HS.InfixApp _ a (HS.QVarOp _ name) b -> do
      ae <- convertExp a
      be <- convertExp b
      var <- resolveQName name
      pure $ App (App (findProof name (Var var)) ae) be
    HS.Paren _ sub -> convertExp sub
    HS.Lambda _ pats sub ->
      Lam
        <$> sequence [ bindVariable name | HS.PVar _ name <- pats ]
        <*> convertExp sub
    HS.Case _ scrut@HS.Var{} alts -> do
      scrut' <- convertExp scrut
      convertAlts scrut' alts
    HS.Case _ scrut alts -> do
      scrut' <- convertExp scrut
      scrutVar <- Variable <$> newName "scrut" <*> exprType scrut'
      def <- convertAlts (Var scrutVar) alts
      return $ Case scrut' scrutVar (Just def) []
    HS.Lit _ (HS.Char _ c _) ->
      pure $ Con charCon `App` Lit (LitChar c)
    HS.Lit _ (HS.Int _ i _) ->
      pure $ Con intCon `App` (Var i64toi32 `App` Lit (LitInt i))
    HS.Lit _ lit -> pure $ convertLiteralToExpr lit
    HS.Tuple  _ HS.Unboxed exprs -> do
      args <- mapM convertExp exprs
      return $ UnboxedTuple args
    HS.Let _ (HS.BDecls _ binds) inExpr -> do
      decls <- mapM convertDecl' binds
      Let (Rec [ (Variable name ty, body)
               | Declaration ty name body <- concat decls ])
          <$> convertExp inExpr
    HS.List (TC.Coerced _ _ _proof) [] ->
      -- return $ convertProof proof (Con nilCon)
      return $ Con nilCon
    HS.Do _ stmts ->
      convertStmts stmts
    _ -> unhandledSyntax expr

convertAlts :: Expr -> [HS.Alt Typed] -> M Expr
convertAlts _scrut [] = error "" -- pure $ Case (Var scrut) scrut Nothing []
convertAlts scrut [HS.Alt _ pat rhs Nothing] =
  convertAltPat scrut Nothing pat =<< convertRhs rhs
convertAlts scrut (HS.Alt _ pat rhs Nothing:alts) = do
  rest <- convertAlts scrut alts
  if isSimplePat pat
    then
      convertAltPat scrut (Just rest) pat =<< convertRhs rhs
    else do
      restBranch <- Variable <$> newName "branch" <*> exprType rest
      e <- convertAltPat scrut (Just $ Var restBranch) pat =<< convertRhs rhs
      return $ Let (NonRec restBranch rest) e
convertAlts _ (alt:_) = unhandledSyntax alt

isSimplePat :: HS.Pat Typed -> Bool
isSimplePat pat =
    case pat of
        HS.PApp _ _name pats     -> all isPVar pats
        HS.PInfixApp _ a _name b -> all isPVar [a,b]
        HS.PVar{}                -> True
        HS.PLit{}                -> True
        HS.PParen _ pat'         -> isSimplePat pat'
        HS.PList _ pats          -> all isPVar pats
        _                        -> False
  where
    isPVar HS.PVar{} = True
    isPVar _         = False

convertAltPat :: Expr -> Maybe Expr -> HS.Pat Typed -> Expr -> M Expr
convertAltPat scrut failBranch pat successBranch =
  case pat of
    HS.PApp _ name pats -> do
      scrut' <- Variable <$> newName "scrut" <*> exprType scrut
      args <- sequence [ Variable (bindName var) <$> lookupType var
                       | HS.PVar _ var <- pats ]
      alt <- Alt <$> (ConPat <$> resolveQName name <*> pure args)
          <*> pure successBranch
      return $ Case scrut scrut' failBranch [alt]
    HS.PInfixApp src a con b -> convertAltPat scrut failBranch (HS.PApp src con [a,b]) successBranch
    HS.PTuple _ HS.Unboxed pats -> do
      scrut' <- Variable <$> newName "scrut" <*> exprType scrut
      args <- sequence [ Variable (bindName var) <$> lookupType var
                       | HS.PVar _ var <- pats ]
      alt <- Alt (UnboxedPat args)
          <$> pure successBranch
      return $ Case scrut scrut' failBranch [alt]
    HS.PWildCard _ ->
      return successBranch
    HS.PVar _ var -> do
      var' <- Variable (bindName var) <$> lookupType var
      if Var var' == scrut
        then return successBranch
        else return $ Let (NonRec var' scrut) successBranch
    -- 0 -> ...
    -- I# i -> case i of
    --            0# -> ...
    HS.PLit _ _sign (HS.Int _ int _) -> do
      scrut' <- Variable <$> newName "scrut" <*> exprType scrut
      intVar <- Variable <$> newName "i" <*> pure i32
      intVar64 <- Variable <$> newName "i64" <*> pure i64
      let alt = Alt (ConPat intCon [intVar]) $
                Case (Var i32toi64 `App` Var intVar) intVar64 failBranch
                [Alt (LitPat (LitInt int)) successBranch]
      return $ Case scrut scrut' Nothing [alt]
    HS.PLit _ _sign lit -> do
      scrut' <- Variable <$> newName "scrut" <*> exprType scrut
      alt <- Alt (LitPat $ convertLiteral lit)
          <$> pure successBranch
      return $ Case scrut scrut' failBranch [alt]
    HS.PParen _ pat' ->
      convertAltPat scrut failBranch pat' successBranch
    HS.PList _ [] -> do
      scrut' <- Variable <$> newName "scrut" <*> exprType scrut
      let alt = Alt (ConPat nilCon []) successBranch
      return $ Case scrut scrut' failBranch [alt]
    _ -> unhandledSyntax pat

_convertAlt :: HS.Alt Typed -> M Alt
_convertAlt alt =
    case alt of
        HS.Alt _ (HS.PApp _ name pats) rhs Nothing -> do
            args <- sequence [ Variable (bindName var) <$> lookupType var
                             | HS.PVar _ var <- pats ]
            Alt <$> (ConPat <$> resolveQName name <*> pure args)
                <*> convertRhs rhs
        HS.Alt _ (HS.PTuple _ HS.Unboxed pats) rhs Nothing -> do
            args <- sequence [ Variable (bindName var) <$> lookupType var
                             | HS.PVar _ var <- pats ]
            Alt (UnboxedPat args)
                <$> convertRhs rhs
        HS.Alt _ (HS.PLit _ _sign lit) rhs Nothing ->
            Alt (LitPat $ convertLiteral lit)
                <$> convertRhs rhs
        -- HS.Alt _ (HS.PVar _ var) rhs Nothing ->
        --     Alt <$> (VarPat <$> (Variable <$> bindName var <*> lookupType var))
        --         <*> convertRhs rhs
        _ -> unhandledSyntax alt

convertLiteralToExpr :: HS.Literal Typed -> Expr
convertLiteralToExpr lit =
    case lit of
        HS.PrimString _ str _ -> Lit $ LitString str
        HS.PrimInt _ int _    -> Lit $ LitInt int
        HS.PrimChar _ char _  -> Lit $ LitChar char
        HS.String _ str _     -> App unpackString (Lit $ LitString str)
        _                     -> unhandledSyntax lit

convertLiteral :: HS.Literal Typed -> Literal
convertLiteral lit =
    case lit of
        HS.PrimString _ str _ -> LitString str
        HS.PrimInt _ int _    -> LitInt int
        HS.PrimChar _ char _  -> LitChar char
        _                     -> unhandledSyntax lit

exprType :: Expr -> M TC.Type
exprType expr =
    case expr of
        Var v -> return (varType v)
        Con c -> return (varType c)
        App a _b -> do
            aType <- exprType a
            case aType of
                TC.TyFun _ ret                       -> return ret
                TC.TyForall [] (_ :=> TC.TyFun _ ret) -> return ret
                TC.TyForall (_:tvs) (_ :=> ty) -> return $ TC.TyForall tvs ([] :=> ty)
                -- _ -> error $ "Unknown type: " ++ show (aType, _b)
                _                                    -> return TC.TyUndefined
        Let _ e -> exprType e
        LetStrict _ _ e -> exprType e
        Case _ _ (Just e) _ -> exprType e
        Case _ _ Nothing (Alt _ e:_) -> exprType e
        _ -> return TC.TyUndefined




getNameIdentifier :: HS.Name l -> String
getNameIdentifier (HS.Ident _ ident)   = ident
getNameIdentifier (HS.Symbol _ symbol) = symbol





-- LHC.Prim builtins
i8, i32, i64, realWorld, io, int32, charTy, intTy, addrTy, ptrTy :: TC.Type
i8 = TC.TyCon $ QualifiedName "LHC.Prim" "I8"
i32 = TC.TyCon $ QualifiedName "LHC.Prim" "I32"
i64 = TC.TyCon $ QualifiedName "LHC.Prim" "I64"
realWorld = TC.TyCon $ QualifiedName "LHC.Prim" "RealWorld#"
io = TC.TyCon $ QualifiedName "LHC.Prim" "IO"
int32 = TC.TyCon $ QualifiedName "LHC.Prim" "Int32"
charTy = TC.TyCon $ QualifiedName "LHC.Prim" "Char"
intTy = TC.TyCon $ QualifiedName "LHC.Prim" "Int"
addrTy = TC.TyCon $ QualifiedName "LHC.Prim" "Addr"
ptrTy = TC.TyCon $ QualifiedName "LHC.Prim" "Ptr"

-- data Int = I# I32
intCon :: Variable
intCon = Variable (Name ["LHC.Prim"] "I#" 0)
  (i32 `TC.TyFun` intTy)

-- data Char = C# I32
charCon :: Variable
charCon = Variable (Name ["LHC.Prim"] "C#" 0)
  (i32 `TC.TyFun` charTy)

listCon :: Variable
listCon = Variable (Name ["@","LHC.Prim"] "List" 0)
  (TC.TyFun TC.TyStar TC.TyStar)

-- data List a = Nil | Cons a (List a)
nilCon :: Variable
nilCon = Variable (Name ["LHC.Prim"] "Nil" 0)
    (TC.TyForall [a] ([] :=> TC.TyList (TC.TyRef a)))
  where
    a = TcVar "a" []

-- data List a = Nil | Cons a (List a)
consCon :: Variable
consCon = Variable (Name ["LHC.Prim"] "Cons" 0)
    (TC.TyForall [a] ([] :=> (TC.TyRef a `TC.TyFun` TC.TyList (TC.TyRef a) `TC.TyFun` TC.TyList (TC.TyRef a))))
  where
    a = TcVar "a" []

unitTCon :: Variable
unitTCon = Variable (Name ["@","LHC.Prim"] "Unit" 0)
  (TC.TyStar)

-- data Unit = Unit
unitCon :: Variable
unitCon = Variable (Name ["LHC.Prim"] "Unit" 0)
  (TC.TyTuple [])

-- newtype IO a = IO (RealWorld# -> (# RealWorld#, a #))
_ioCon :: Variable
_ioCon = Variable (Name ["LHC.Prim"] "IO" 0)
    $ TC.TyForall [a] ([] :=> ((realWorld `TC.TyFun` TC.TyUnboxedTuple [realWorld, TC.TyRef a]) `TC.TyFun` TC.TyApp io (TC.TyRef a)))
  where
    a = TcVar "a" []
  -- (RealWorld# -> (# RealWorld#, retType #)) -> IO retType

int32Con, i32toi64, i64toi32 :: Variable
-- data Int32 = Int32 I32
int32Con = Variable (Name ["LHC.Prim"] "Int32" 0)
  (i32 `TC.TyFun` int32)

i32toi64 = Variable (Name ["LHC.Prim"] "i32toi64" 0) (TC.TyFun i32 i64)
i64toi32 = Variable (Name ["LHC.Prim"] "i64toi32" 0) (TC.TyFun i64 i32)

ptrCon :: Variable
ptrCon = Variable (Name ["LHC.Prim"] "Ptr" 0) $
    TC.TyForall [a] ([] :=> (TC.TyApp addrTy aTy `TC.TyFun` TC.TyApp ptrTy aTy))
  where
    a = TcVar "a" []
    aTy = TC.TyRef a


unhandledSyntax :: (Typeable a, HS.Pretty a) => a -> b
unhandledSyntax val = error $
  "Unhandled syntax: (type: "++ show (typeOf val) ++ ")\n" ++
  HS.prettyPrint val

{-# LANGUAGE NoMonadFailDesugaring #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler.CoreToBedrock where


import           Data.Bedrock                      as Bedrock
import           Data.Bedrock.Transform            (freeVariables)
import           Language.Haskell.Crux             hiding (Foreign (..),
                                                    Name (..),
                                                    NodeDefinition (..),
                                                    UnboxedPat, Variable (..))
import qualified Language.Haskell.Crux             as Core
import           Language.Haskell.TypeCheck.Pretty (pretty)

import           Control.Monad.Reader
import           Control.Monad.RWS                 (MonadState (..), RWS,
                                                    execRWS)
import           Control.Monad.Writer              (MonadWriter (..))
import           Data.Char                         (ord)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe
import qualified Data.Set                          as Set
import qualified LLVM.AST                          as LLVM (Type (..))
import           LLVM.AST.AddrSpace
import qualified LLVM.AST.Type                     as LLVM

import           Language.Haskell.Scope            (QualifiedName (..))
import qualified Language.Haskell.TypeCheck        as TC (Proof, Qualified (..),
                                                          Type (..))

-- import Debug.Trace

entrypointName :: Name
entrypointName = Name ["Main"] "entrypoint" 0

convert :: Core.Module -> Bedrock.Module
convert m = Bedrock.Module
  { modForeigns  = map convertForeign (Core.cruxForeigns m)
  , nodes        = map convertNodeDefinition (Core.cruxNodes m)
  , modLayouts   = []
  , entryPoint   = entrypointName
  , functions    = fns
  , modNamespace = AvailableNamespace ns ns ns ns }
  where
    (fns, ns) = runM (convertModule m) (Core.cruxNewTypes m) 0

convertForeign :: Core.Foreign -> Foreign
convertForeign core = Foreign
  { foreignName = Core.foreignName core
  , foreignReturn = Core.foreignReturn core
  , foreignArguments = Core.foreignArguments core }

convertName :: Core.Name -> Name
convertName (Core.Name prefix ident uniq) = Name prefix ident uniq

convertNodeDefinition :: Core.NodeDefinition -> NodeDefinition
convertNodeDefinition (Core.NodeDefinition (Core.Variable name ty)) =
    NodeDefinition (convertName name) (convertTcTypes ty)

convertModule :: Core.Module -> M ()
convertModule m = setArities arities $ do
    mapM_ convertDecl (Core.cruxDecls m)
  where
    arities =
      [ (convertName fn, arity)
      | Core.Declaration _ty fn expr <- cruxDecls m
      , let arity = case expr of
                      Lam vars _ -> length vars
                      -- WithCoercion _ (Lam vars _) -> length vars
                      _          -> 0 ] ++
      [ (convertName name, length $ convertTcTypes ty)
      | Core.NodeDefinition (Core.Variable name ty) <- cruxNodes m ] ++
      [ (convertName $ Core.varName con, 1)
      | Core.IsNewType con <- cruxNewTypes m ]

data Env = Env
    { envArity    :: Map Name Int
    , envRoot     :: Name
    , envLocation :: [String]
    , envNewTypes :: [NewType]
    }
newtype M a = M { unM :: RWS Env [Function] Int a }
    deriving
        ( Monad, MonadWriter [Function], MonadReader Env
        , MonadState Int
        , Applicative, Functor )
runM ::  M a -> [NewType] -> Int -> ([Function], Int)
runM action newtypes ns = (fns, ns')
  where
    (ns',fns) = execRWS (unM action) env ns
    env = Env
        { envArity    = Map.empty
        , envRoot     = error "envRoot"
        , envLocation = []
        , envNewTypes = newtypes
        }

setArity :: Name -> Int -> M a -> M a
setArity fn arity = local $ \env -> env
    { envArity = Map.insert fn arity (envArity env) }

setArities :: [(Name, Int)] -> M a -> M a
setArities arities = local $ \env -> env
    { envArity = Map.union (Map.fromList arities) (envArity env) }

lookupArity :: Name -> M (Maybe Int)
lookupArity fn = asks $ Map.lookup fn . envArity

requireArity :: Name -> M Int
requireArity name = do
  mbArity <- lookupArity name
  case mbArity of
    Nothing    -> error $ "No arity for: " ++ show name
    Just arity -> pure arity

newName :: [String] -> String -> M Name
newName orig ident = do
  ns <- get
  put (ns+1)
  return $ Name orig ident ns

newVariable :: [String] -> String -> Type -> M Variable
newVariable orig ident ty = do
  name <- newName orig ident
  return $ Variable name ty

pushFunction :: [Variable] -> Either String Name -> Bedrock.Block -> M (Name, [Variable])
pushFunction regArgs origin body = do
  let free = freeVariables body Set.\\ Set.fromList regArgs
      args = Set.toList free ++ regArgs

  Name orig ident _ <- asks envRoot
  name <- case origin of
              Left tag -> newName (orig++[ident]) tag
              Right n  -> return n
  let fn = Function
          { fnName = name
          , fnAttributes = []
          , fnArguments = args
          , fnResults = blockType body
          , fnBody = body }
  tell [fn]
  return (name, Set.toList free)

{-
data Type
  = NodePtr
  | Node
  | StaticNode NodeSize
  | Primitive CType
  | LLVMPrimitive LLVM.Type
  | FramePtr
-}
blockType :: Bedrock.Block -> [Type]
blockType block =
  case block of
    Bedrock.Case _ (Just defBlock) _ -> blockType defBlock
    Bedrock.Case _ _ (Alternative _ rest:_) -> blockType rest
    Bedrock.Bind _ _ rest -> blockType rest
    Bedrock.Return vars -> map Bedrock.variableType vars
    _ -> error $ "Can't find block type: " ++ show block

-- XXX: Lookup the kind to find the bedrock type.
convertVariable :: Core.Variable -> M Variable
convertVariable var = return $
    Variable (convertName $ Core.varName var) (convertTcType (Core.varType var))

convertTcTypes :: TC.Type -> [Type]
convertTcTypes tcTy =
  case tcTy of
    TC.TyFun a b                   -> convertTcType a : convertTcTypes b
    TC.TyApp{}                     -> []
    TC.TyCon{}                     -> []
    TC.TyStar{}                    -> []
    TC.TyForall _tvs (_ TC.:=> ty) -> convertTcTypes ty
    _                              -> error $ "convertTcTypes: " ++ show tcTy

convertTcType :: TC.Type -> Type
convertTcType tcTy =
  case tcTy of
    TC.TyApp (TC.TyCon qname) sub
        | qname == QualifiedName "LHC.Prim" "Addr"
            -> case convertTcType sub of
                 Primitive prim -> Primitive (LLVM.ptr prim)
                 _              -> Primitive (LLVM.ptr LLVM.i8) -- FIXME: Ugly hack because types aren't right
                 -- _ -> error $ "CoreToBedrock: Addr must be primitive: " ++ show sub
    TC.TyCon qname
        | qname == QualifiedName "LHC.Prim" "I8"
             -> Primitive LLVM.i8
    TC.TyCon qname
        | qname == QualifiedName "LHC.Prim" "I32"
             -> Primitive LLVM.i32
    TC.TyCon qname
        | qname == QualifiedName "LHC.Prim" "I64"
             -> Primitive LLVM.i64
    TC.TyCon qname
        | qname == QualifiedName "LHC.Prim" "RealWorld#"
         -> Primitive LLVM.VoidType
    TC.TyFun{} -> NodePtr
    TC.TyRef{} -> NodePtr
    TC.TyCon{} -> NodePtr
    TC.TyApp{} -> NodePtr
    -- TC.TyUndefined -> NodePtr
    _ -> NodePtr
    -- _ -> error $ "CoreToBedrock: Unknown type: " ++ show tcTy

setProgramExit :: Block -> Block
setProgramExit block =
  case block of
    Bedrock.Case scrut mbDefault alts ->
      Bedrock.Case scrut mbDefault
        [ Alternative pattern (setProgramExit branch)
        | Alternative pattern branch <- alts ]
    Bind binds expr rest -> Bind binds expr (setProgramExit rest)
    Recursive binds rest -> Recursive binds (setProgramExit rest)
    Return{} -> Exit
    Raise{} -> error "setProgramExit"
    TailCall{} -> error "setProgramExit"
    Invoke{} -> error "setProgramExit"
    -- InvokeHandler{} -> error "setProgramExit"
    Exit -> Exit
    Panic msg -> Panic msg

-- coreType :: Core.Expr -> Type
-- coreType expr =
--   case expr of
--     Var var -> convertResultType (varType var)
--     Con Name
--     UnboxedTuple [Expr]
--     Lit Literal
--     WithExternal Variable String [Variable] Variable Expr
--     ExternalPure Variable String [Variable] Expr
--     App Expr Expr
--     Lam [Variable] Expr
--     Let LetBind Expr
--     LetStrict Variable Expr Expr
--     Case Expr Variable (Maybe Expr) [Alt]
--     Cast Expr TcType
--     Id
--     WithCoercion Coercion Expr

convertResultType :: [Variable] -> TC.Type -> M [Type]
convertResultType args tcTy =
    case collect [] tcTy of
      (TC.TyCon qname, [sub])
        | qname == QualifiedName "LHC.Prim" "Addr"
            -> case convertTcType sub of
                 Primitive prim -> return [Primitive (LLVM.PointerType prim (AddrSpace 0))]
                 _ -> error $ "CoreToBedrock: Addr must be primitive: " ++ show sub
      (TC.TyCon qname, [])
        | qname == QualifiedName "LHC.Prim" "I8"
             -> return [Primitive $ LLVM.IntegerType 8]
      (TC.TyCon qname, [])
        | qname == QualifiedName "LHC.Prim" "I32"
             -> return [Primitive $ LLVM.IntegerType 32]
      (TC.TyCon qname, [])
        | qname == QualifiedName "LHC.Prim" "I64"
             -> return [Primitive $ LLVM.IntegerType 64]
      (TC.TyCon qname, [])
        | qname == QualifiedName "LHC.Prim" "RealWorld#"
             -> return [Primitive LLVM.VoidType]
      (TC.TyCon qname, conArgs) -> do
        mbExpanded <- expandNewType qname conArgs
        case mbExpanded of
          Nothing -> return [NodePtr]
          Just ty -> convertResultType args ty
      (TC.TyFun _ a,[]) ->
        case args of
          []        -> return [NodePtr]
          (_:argss) -> convertResultType argss a
      -- TcForall _ (_ :=> ty) -> convertResultType ty
      (TC.TyUnboxedTuple tys,[]) -> concat <$> mapM (convertResultType []) tys
      (TC.TyForall _ ([] TC.:=> ty), []) -> convertResultType args ty
      _ -> return [NodePtr]
  where
    collect acc (TC.TyApp a b) = collect (b:acc) a
    collect acc ty             = (ty, reverse acc)

expandNewType :: QualifiedName -> [TC.Type] -> M (Maybe TC.Type)
expandNewType qname args = do
  newtypes <- asks envNewTypes
  return $ listToMaybe
    [ ntSubst args
    | nt <- newtypes
    , let (ntName, ntSubst) = newtypeSubstitution nt
    , ntName == qname ]

-- newtype LHC.Prim.IO:∀ a. (LHC.Prim.RealWorld# →  (# LHC.Prim.RealWorld#, a #)) →  LHC.Prim.IO a
-- substitution:
--   \[a] -> LHC.Prim.RealWorld# →  (# LHC.Prim.RealWorld#, a #)
newtypeSubstitution :: NewType -> (QualifiedName, [TC.Type] -> TC.Type)
newtypeSubstitution (IsNewType var) =
    split (Core.varType var)
  where
    split (TC.TyForall _tcvars ([] TC.:=> ty)) = split ty
    split (TC.TyFun a b) =
      case collect [] b of
        (TC.TyCon newtypeName, conArgs) -> (newtypeName, \args -> subst (zip conArgs args) a)
        _ -> error "Weird newtype"
    split _ = error "Urk: Weird newtype"
    subst s ty =
      case lookup ty s of
        Just ty' -> ty'
        Nothing -> case ty of
          TC.TyApp a b  -> TC.TyApp (subst s a) (subst s b)
          TC.TyFun a b  -> TC.TyFun (subst s a) (subst s b)
          TC.TyTuple ls -> TC.TyTuple (map (subst s) ls)
          _             -> ty
    collect acc (TC.TyApp a b) = collect (b:acc) a
    collect acc ty             = (ty, reverse acc)

-- XXX: Move this function.
isPrimitive :: Type -> Bool
isPrimitive Primitive{} = True
isPrimitive _           = False

convertDecl :: Core.Declaration -> M ()
convertDecl (Core.Declaration ty name (Lam vars expr)) = do
  vars' <- mapM convertVariable vars
  body <- local (\env -> env{envRoot = convertName name}) $
          convertExpr False expr (pure . Return)
  retTys <- convertResultType vars' ty
  let fn = Function
          { fnName = convertName name
          , fnAttributes = []
          , fnArguments = vars'
          , fnResults = retTys
          , fnBody = if convertName name == entrypointName
                      then setProgramExit body
                      else body
          }
  tell [fn]
-- convertDecl (Core.Decl _ty name (WithCoercion _ expr)) =
--   convertDecl (Core.Decl _ty name expr)
convertDecl (Core.Declaration ty name expr) = do
  body <- local (\env -> env{envRoot = convertName name}) $
          convertExpr False expr (pure . Return)
  retTys <- convertResultType [] ty
  let fn = Function
          { fnName = convertName name
          , fnAttributes = []
          , fnArguments = []
          , fnResults = retTys
          , fnBody = if convertName name == entrypointName
                      then setProgramExit body
                      else body
          }
  tell [fn]

--convertExpr :: Core.Expr -> (Variable -> M Bedrock.Block) -> M Bedrock.Block
--convertExpr expr rest =
--    convertExprLazy expr $ \val -> do
--    let tmp = Variable (Name [] "strict" 0) Node
--        attrs = MemAttributes False Nothing
--    Bind [tmp] (Eval val)
--        <$> rest tmp

setOrigin :: M a -> M a
setOrigin = local $ \env ->
  let Name orig ident _ = envRoot env
  in env { envLocation = orig ++ [ident] }

-- applyCoercion :: Coercion -> TcType -> TcType
-- applyCoercion CoerceId ty = ty
-- applyCoercion (CoerceAbs vars) ty = error $ "Weird Ap coercion: " ++ show (vars, ty)
-- applyCoercion (CoerceAp tys) (TcForall vars ([] :=> ty)) = worker (zip vars tys) ty
--   where
--     worker subst ty =
--       case ty of
--         TcForall{} -> error $ "applyCoercion: TcForall"
--         TcFun a b -> TcFun (worker subst a) (worker subst b)
--         TcApp a b -> TcApp (worker subst a) (worker subst b)
--         TcRef var ->
--           case lookup var subst of
--             Nothing -> ty
--             Just ty' -> ty
--         TcCon con -> TcCon con
--         TcMetaVar meta -> TcMetaVar meta
--         TcUnboxedTuple tys -> TcUnboxedTuple (map (worker subst) tys)
--         TcTuple tys -> TcTuple (map (worker subst) tys)
--         TcList ty -> TcList (worker subst ty)
--         TcUndefined -> TcUndefined

collectApps :: Core.Expr -> (Core.Expr, Maybe TC.Proof, [Core.Expr])
collectApps = worker []
  where
    worker acc expr =
        case expr of
            App a b -> worker (b:acc) a
            _       -> (expr, Nothing, acc)

convertExpr :: Bool -> Core.Expr -> ([Variable] -> M Bedrock.Block) -> M Bedrock.Block
convertExpr lazy expr rest =
  case expr of
    UnboxedTuple args -> convertExprs args rest
    _ | (Con con, _coercion, args) <- collectApps expr ->
      convertExprs args $ \args' -> do
        tmp <- newVariable [] "con" NodePtr
        let name = convertName $ Core.varName con
        arity <- requireArity name
        Bind [tmp] (Store (ConstructorName name (arity-length args)) args')
          <$> rest [tmp]
    _ | (Var v, _coercion, args) <- collectApps expr, lazy ->
      convertExprs args $ \args' -> do
        v' <- convertVariable v
        let fn = variableName v'
        fnRetTys <- convertResultType args' (Core.varType v)
        mbArity <- lookupArity fn
        case mbArity of
          Just arity | arity == length args' && (length fnRetTys > 1 || all isPrimitive fnRetTys) -> do
            tmp <- mapM (newVariable [] "thunk") fnRetTys
            Bind tmp (Application fn args')
              <$> rest tmp
          Just arity | arity >= length args' -> do
            tmp <- newVariable [] "thunk" NodePtr
            Bind [tmp] (Store (FunctionName fn (arity-length args')) args')
              <$> rest [tmp]
          Nothing | null args -> do
            rest [v']
          _Nothing -> do
            body <- do
              tmp <- deriveVariable v' "eval" NodePtr
              Bind [tmp] (Eval v') <$>
                applyMany fnRetTys tmp args' (pure . Return)
            (node, nodeArgs) <- pushFunction [] (Left "ap") body
            tmp <- newVariable [] "ap" NodePtr
            Bind [tmp] (Store (FunctionName node 0) nodeArgs)
              <$> rest [tmp]
    _ | (Var v, _coercion, args) <- collectApps expr, not lazy ->
      convertExprs args $ \args' -> do
        v' <- convertVariable v
        let fn = variableName v'
        mbArity <- lookupArity fn
        case mbArity of
          Nothing | null args, variableType v' /= NodePtr ->
            rest [v']
          Nothing -> do
            fnRetTys <- convertResultType args' (Core.varType v)
            tmp <- deriveVariable v' "eval" NodePtr
            Bind [tmp] (Eval v')
              <$> applyMany fnRetTys tmp args' rest
          Just arity | arity == length args' -> do
            fnRetTys <- convertResultType args' (Core.varType v)
            tmp <- mapM (newVariable [] "thunk") fnRetTys
            Bind tmp (Application fn args')
              <$> rest tmp
          Just arity | arity < length args' -> do
            [fnRetTy] <- convertResultType (take arity args') ({-applyCoercion coercion $-} Core.varType v)
            finalTys <- convertResultType args' ({-applyCoercion coercion $-} Core.varType v)
            tmp <- newVariable [] "ret" fnRetTy
            -- fetched <- newVariable [] "thunk" Node
            Bind [tmp] (Application fn (take arity args')) <$>
              applyMany finalTys tmp (drop arity args') rest
          Just arity -> do
            tmp <- newVariable [] "thunk" NodePtr
            Bind [tmp] (Store (FunctionName fn (arity-length args')) args')
              <$> rest [tmp]
    Core.Lit (Core.LitString str) -> do
        tmp <- newVariable [] "lit" (Primitive $ LLVM.ptr LLVM.i8)
        Bind [tmp] (Bedrock.Literal (LiteralString str))
            <$> rest [tmp]
    Core.Lit (Core.LitI64 int) -> do
        tmp <- newVariable [] "int" (Primitive LLVM.i64)
        Bind [tmp] (Bedrock.Literal (LiteralInt $ fromIntegral int))
            <$> rest [tmp]
    Core.Lit (Core.LitI32 int) -> do
        tmp <- newVariable [] "int" (Primitive LLVM.i32)
        Bind [tmp] (Bedrock.Literal (LiteralInt $ fromIntegral int))
            <$> rest [tmp]
    Core.Lit (Core.LitI16 int) -> do
        tmp <- newVariable [] "int" (Primitive LLVM.i16)
        Bind [tmp] (Bedrock.Literal (LiteralInt $ fromIntegral int))
            <$> rest [tmp]
    Core.Lit (Core.LitI8 int) -> do
        tmp <- newVariable [] "int" (Primitive LLVM.i8)
        Bind [tmp] (Bedrock.Literal (LiteralInt $ fromIntegral int))
            <$> rest [tmp]
    Core.Lit (Core.LitChar c) -> do
        tmp <- newVariable [] "char" (Primitive LLVM.i32)
        Bind [tmp] (Bedrock.Literal (LiteralInt $ fromIntegral $ ord c))
            <$> rest [tmp]
    Core.Lit Core.LitVoid -> do
        tmp <- newVariable [] "void" (Primitive LLVM.VoidType)
        Bind [tmp] (Bedrock.Literal (LiteralInt 0))
            <$> rest [tmp]
    Lam v sub | lazy -> do
      v' <- mapM convertVariable v
      (node,nodeArgs) <- do
          body <- convertExpr False sub (pure . Return)
          pushFunction v' (Left "lambda") body
      tmp <- newVariable [] "thunk" NodePtr
      Bind [tmp] (Store (FunctionName node (length v)) nodeArgs)
          <$> rest [tmp]
    Core.Case scrut _var Nothing [Core.Alt (Core.UnboxedPat binds) branch] | not lazy ->
      convertExpr False scrut $ \vals -> do
        binds' <- mapM convertVariable binds
        let worker [] []         = convertExpr False branch rest
            worker (x:xs) (y:ys) = Bind [x] (TypeCast y) <$> worker xs ys
            worker [] _          = error "Not enough binds"
            worker _ []          = error "Not enough vals"
        worker binds' vals
    Core.Case scrut var Nothing alts | not lazy ->
      convertExpr False scrut $ \[val] ->
        if variableType val == NodePtr
          then do
            var' <- convertVariable var
            tmp <- deriveVariable val "node" Node
            Bind [var'] (Bedrock.TypeCast val) <$>
              Bind [tmp] (Bedrock.Fetch val) <$>
                (Bedrock.Case tmp Nothing <$>
                    mapM convertAlt alts)
          else
            Bedrock.Case val Nothing <$>
                  mapM convertAlt alts
    Core.Case scrut var (Just def) alts | not lazy ->
        convertExpr False scrut $ \[val] -> do
            var' <- convertVariable var
            defExpr <- convertExpr False def (pure . Return)
            if isPrimitive (variableType var')
                then
                    Bind [var'] (Bedrock.TypeCast val) <$>
                        Bedrock.Case val (Just defExpr) <$>
                            mapM convertAlt alts
                else do
                  tmp <- deriveVariable val "node" Node
                  Bind [tmp] (Bedrock.Fetch val) <$>
                    Bedrock.Case tmp (Just defExpr) <$>
                        mapM convertAlt alts
    Convert e ty ->
      convertExpr False e $ \[val] -> do
        var <- deriveVariable val "val" (convertTcType ty)
        Bind [var] (Bedrock.TypeCast val) <$> rest [var]
    WithExternal binder retS external args _st scoped ->
      convertExprs args $ \args' -> do
        binder' <- convertVariable binder
        retS' <- convertVariable retS
        Bind [binder', retS'] (CCall external args')
            <$> convertExpr False scoped rest

    ExternalPure binder external args scoped ->
      convertExprs args $ \args' -> do
        binder' <- convertVariable binder
        Bind [binder'] (CCall external args')
            <$> convertExpr False scoped rest

    Let (NonRec name e1) e2 ->
      convertExpr True e1 $ \[val] -> do
          name' <- convertVariable name
          Bind [name'] (TypeCast val) <$> convertExpr lazy e2 rest

    -- Allocate the ptrs with the right sizes.
    -- Then fill in the data.
    -- x <- allocate (Cons _ _)
    -- write(x, Cons 1 x)
    Let (Rec []) e2 -> convertExpr lazy e2 rest
    Let (Rec ((name,e1):binds)) e2 -> do
      name' <- convertVariable name
      (convertExprLazy e1 $ \term val ->
        Bind [name'] (TypeCast val) . Bind [] term <$> convertExpr lazy (Let (Rec binds) e2) rest)

    LetStrict name e1 e2 ->
      convertExpr False e1 $ \[val] -> do
          name' <- convertVariable name
          Bind [name'] (TypeCast val) <$> convertExpr lazy e2 rest

    Core.Case{} | lazy -> do
      body <- convertExpr False expr (pure . Return)
      (node, nodeArgs) <- pushFunction [] (Left "thunk") body
      tmp <- newVariable [] "thunk" NodePtr
      Bind [tmp] (Store (FunctionName node 0) nodeArgs)
        <$> rest [tmp]
    Core.Cast ->
      rest []
    _ | lazy -> error $ "C->B convertExpr: (lazy: "++ show lazy ++ ")\n" ++ show (pretty expr)
    _ -> -- not lazy
      convertExpr True expr $ \[val] -> do
      tmp <- deriveVariable val "eval" NodePtr
      Bind [tmp] (Eval val)
          <$> rest [tmp]

splitStore con args rest = do
  tmp <- newVariable [] "tmp" NodePtr
  Bind [tmp] (StoreAlloc con (map variableType args))
    <$> rest (StoreWrite tmp args) tmp

convertExprLazy :: Core.Expr -> (Bedrock.Expression -> Variable -> M Bedrock.Block) -> M Bedrock.Block
convertExprLazy expr rest =
  case expr of
    _ | (Con con, _coercion, args) <- collectApps expr ->
      convertExprs args $ \args' -> do
        tmp <- newVariable [] "con" NodePtr
        let name = convertName $ Core.varName con
        arity <- requireArity name
        let con = ConstructorName name (arity-length args)
        splitStore con args' rest
    _ | (Var v, _coercion, args) <- collectApps expr ->
      convertExprs args $ \args' -> do
        v' <- convertVariable v
        let fn = variableName v'
        fnRetTys <- convertResultType args' (Core.varType v)
        mbArity <- lookupArity fn
        case mbArity of
          Just arity | arity >= length args' -> do
            let con = FunctionName fn (arity-length args')
            splitStore con args' rest
          Nothing | null args -> do
            rest (Literal (LiteralInt 0)) v'
          _Nothing -> do
            body <- do
              tmp <- deriveVariable v' "eval" NodePtr
              Bind [tmp] (Eval v') <$>
                applyMany fnRetTys tmp args' (pure . Return)
            (node, nodeArgs) <- pushFunction [] (Left "ap") body
            let con = FunctionName node 0
            splitStore con nodeArgs rest
    Core.Lit (Core.LitString str) -> do
        tmp <- newVariable [] "lit" (Primitive $ LLVM.ptr LLVM.i8)
        Bind [tmp] (Bedrock.Literal (LiteralString str))
            <$> rest (Literal (LiteralInt 0)) tmp
    Core.Lit (Core.LitI64 int) -> do
        tmp <- newVariable [] "int" (Primitive LLVM.i64)
        Bind [tmp] (Bedrock.Literal (LiteralInt $ fromIntegral int))
            <$> rest (Literal (LiteralInt 0)) tmp
    Core.Lit (Core.LitI32 int) -> do
        tmp <- newVariable [] "int" (Primitive LLVM.i32)
        Bind [tmp] (Bedrock.Literal (LiteralInt $ fromIntegral int))
            <$> rest (Literal (LiteralInt 0)) tmp
    Core.Lit (Core.LitChar c) -> do
        tmp <- newVariable [] "char" (Primitive LLVM.i32)
        Bind [tmp] (Bedrock.Literal (LiteralInt $ fromIntegral $ ord c))
            <$> rest (Literal (LiteralInt 0)) tmp
    Core.Lit Core.LitVoid -> do
        tmp <- newVariable [] "void" (Primitive LLVM.VoidType)
        Bind [tmp] (Bedrock.Literal (LiteralInt 0))
            <$> rest (Literal (LiteralInt 0)) tmp
    Lam v sub -> do
      v' <- mapM convertVariable v
      (node,nodeArgs) <- do
          body <- convertExpr False sub (pure . Return)
          pushFunction v' (Left "lambda") body
      tmp <- newVariable [] "thunk" NodePtr
      let con = FunctionName node (length v)
      splitStore con nodeArgs rest
    Convert e ty ->
      error "Convert in lazy mode."
    WithExternal binder retS external args _st scoped ->
      error "WithExternal in lazy mode."

    ExternalPure binder external args scoped ->
      error "ExternalPure in lazy mode."

    Let (NonRec name e1) e2 ->
      convertExprLazy e1 $ \term val -> do
          name' <- convertVariable name
          Bind [name'] (TypeCast val) . Bind [] term
            <$> convertExprLazy e2 rest

    Let (Rec []) e2 -> convertExprLazy e2 rest
    Let (Rec ((name,e1):binds)) e2 -> do
      name' <- convertVariable name
      Recursive [name'] <$>
        (convertExprLazy e1 $ \term val ->
          Bind [name'] (TypeCast val) <$> convertExprLazy (Let (Rec binds) e2) rest)

    LetStrict name e1 e2 -> error "LetStrict in lazy mode. Hoist to fn."

    Core.Case{} -> do
      body <- convertExpr False expr (pure . Return)
      (node, nodeArgs) <- pushFunction [] (Left "thunk") body
      tmp <- newVariable [] "thunk" NodePtr
      let con = (FunctionName node 0)
      splitStore con nodeArgs rest
    _ -> error $ "C->B convertExprLazy: \n" ++ show (pretty expr)

convertExprs :: [Core.Expr] -> ([Variable] -> M Bedrock.Block) -> M Bedrock.Block
convertExprs [] fn = fn []
convertExprs (x:xs) fn =
    convertExpr True x $ \v ->
    convertExprs xs $ \vs ->
    fn (v ++ vs)

applyMany :: [Type] -> Variable -> [Variable] -> ([Variable] -> M Block) -> M Block
applyMany _finalTy node [] fn = fn [node]
applyMany finalTy node [x] fn = do
    ret <- mapM (deriveVariable node "apply") finalTy
    Bind ret (Apply node x)
        <$> fn ret
applyMany finalTy node (x:xs) fn = do
    ret <- deriveVariable node "apply" NodePtr
    Bind [ret] (Apply node x)
        <$> applyMany finalTy ret xs fn

deriveVariable :: Variable -> String -> Type -> M Variable
deriveVariable (Variable (Name orig ident _) _) tag ty = do
    path <- asks envLocation
    name <- newName (path ++ orig ++ [ident]) tag
    return $ Variable name ty

convertAlt :: Alt -> M Bedrock.Alternative
convertAlt (Alt pattern branch) =
    case pattern of
        ConPat con args -> do
            args' <- mapM convertVariable args
            let name = convertName $ Core.varName con
            Alternative (NodePat (ConstructorName name 0) args')
                <$> convertExpr False branch (pure . Return)
        Core.LitPat lit ->
            Alternative
                <$> (Bedrock.LitPat <$> convertLiteral lit)
                <*> convertExpr False branch (pure . Return)
        Core.UnboxedPat args -> do
            args' <- mapM convertVariable args
            Alternative (NodePat UnboxedTupleName args')
                    <$> convertExpr False branch (pure . Return)
        -- Core.VarPat var -> do
        --     var' <- convertVariable var
        --     bind [var'] $
        --         Alternative (Bedrock.VarPat var')
        --             <$> convertExpr False branch (pure . Return)

convertLiteral :: Core.Literal -> M Bedrock.Literal
convertLiteral lit = pure $
    case lit of
        LitChar c   -> LiteralInt (fromIntegral $ ord c)
        LitString s -> LiteralString s
        LitI8 i     -> LiteralInt (fromIntegral i)
        LitI16 i    -> LiteralInt (fromIntegral i)
        LitI32 i    -> LiteralInt (fromIntegral i)
        LitI64 i    -> LiteralInt (fromIntegral i)
        LitVoid     -> LiteralInt 0
        _           -> error "Urk: literals"
        -- LitWord Integer
        -- LitFloat Rational
        -- LitDouble Rational

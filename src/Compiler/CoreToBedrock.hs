{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler.CoreToBedrock where


import           Compiler.Core                    hiding (NodeDefinition (..),
                                                   UnboxedPat, Variable (..))
import qualified Compiler.Core                    as Core
import           Data.Bedrock                     as Bedrock
import           Data.Bedrock.Misc
import           Data.Bedrock.Transform           (freeVariables)

import           Control.Monad.Reader
import           Control.Monad.RWS                (MonadState (..), RWS,
                                                   execRWS)
import           Control.Monad.Writer             (MonadWriter (..))
import           Data.Char                        (ord)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import qualified Data.Set                         as Set
import Data.Maybe

import qualified Language.Haskell.TypeCheck as TC (Type (..), Qualified(..), Proof)
import           Language.Haskell.Scope (QualifiedName(..))

-- import Debug.Trace

entrypointName :: Name
entrypointName = Name ["Main"] "entrypoint" 0

convert :: Core.Module -> Bedrock.Module
convert m = Bedrock.Module
  { modForeigns  = Core.coreForeigns m
  , nodes        = map convertNodeDefinition (Core.coreNodes m)
  , entryPoint   = entrypointName
  , functions    = fns
  , modNamespace = ns }
  where
    (fns, ns) = runM (convertModule m) (Core.coreNewTypes m) (Core.coreNamespace m)

convertNodeDefinition :: Core.NodeDefinition -> NodeDefinition
convertNodeDefinition (Core.NodeDefinition name tys) =
    NodeDefinition name (map convertTcType tys)

convertModule :: Core.Module -> M ()
convertModule m = setArities arities $ do
    mapM_ convertDecl (Core.coreDecls m)
  where
    arities =
      [ (fn, arity)
      | Core.Decl _ty fn expr <- coreDecls m
      , let arity = case expr of
                      Lam vars _ -> length vars
                      -- WithCoercion _ (Lam vars _) -> length vars
                      _ -> 0 ] ++
      [ (name, length args)
      | Core.NodeDefinition name args <- coreNodes m ] ++
      [ (Core.varName con, 1)
      | Core.IsNewType con <- coreNewTypes m ]

data Env = Env
    { envArity    :: Map Name Int
    , envRoot     :: Name
    , envLocation :: [String]
    , envNewTypes :: [NewType]
    }
newtype M a = M { unM :: RWS Env [Function] AvailableNamespace a }
    deriving
        ( Monad, MonadWriter [Function], MonadReader Env
        , MonadState AvailableNamespace
        , Applicative, Functor )
runM ::  M a -> [NewType] -> AvailableNamespace -> ([Function], AvailableNamespace)
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

newName :: [String] -> String -> M Name
newName orig ident = do
  ns <- get
  let (idNum, ns') = newGlobalID ns
  put ns'
  return $ Name orig ident idNum

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
              Right n -> return n
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
    Variable (Core.varName var) (convertTcType (Core.varType var))

convertTcType :: TC.Type -> Type
convertTcType tcTy =
  case tcTy of
    TC.TyApp (TC.TyCon qname) sub
        | qname == QualifiedName "LHC.Prim" "Addr"
            -> case convertTcType sub of
                 Primitive prim -> Primitive (CPointer prim)
                 _ -> error $ "CoreToBedrock: Addr must be primitive: " ++ show sub
    TC.TyCon qname
        | qname == QualifiedName "LHC.Prim" "I8"
             -> Primitive I8
    TC.TyCon qname
        | qname == QualifiedName "LHC.Prim" "I32"
             -> Primitive I32
    TC.TyCon qname
        | qname == QualifiedName "LHC.Prim" "I64"
             -> Primitive I64
    TC.TyCon qname
        | qname == QualifiedName "LHC.Prim" "RealWorld#"
         -> Primitive CVoid
    TC.TyFun{} -> NodePtr
    TC.TyRef{} -> NodePtr
    TC.TyCon{} -> NodePtr
    TC.TyApp{} -> NodePtr
    TC.TyUndefined -> NodePtr
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
                 Primitive prim -> return [Primitive (CPointer prim)]
                 _ -> error $ "CoreToBedrock: Addr must be primitive: " ++ show sub
      (TC.TyCon qname, [])
        | qname == QualifiedName "LHC.Prim" "I8"
             -> return [Primitive I8]
      (TC.TyCon qname, [])
        | qname == QualifiedName "LHC.Prim" "I32"
             -> return [Primitive I32]
      (TC.TyCon qname, [])
        | qname == QualifiedName "LHC.Prim" "I64"
             -> return [Primitive I64]
      (TC.TyCon qname, [])
        | qname == QualifiedName "LHC.Prim" "RealWorld#"
             -> return [Primitive CVoid]
      (TC.TyCon qname, conArgs) -> do
        mbExpanded <- expandNewType qname conArgs
        case mbExpanded of
          Nothing -> return [NodePtr]
          Just ty -> convertResultType args ty
      (TC.TyFun _ a,[]) ->
        case args of
          [] -> return [NodePtr]
          (_:argss) -> convertResultType argss a
      -- TcForall _ (_ :=> ty) -> convertResultType ty
      (TC.TyUnboxedTuple tys,[]) -> concat <$> mapM (convertResultType []) tys
      (TC.TyForall _ ([] TC.:=> ty), []) -> convertResultType args ty
      _ -> return [NodePtr]
  where
    collect acc (TC.TyApp a b) = collect (b:acc) a
    collect acc ty = (ty, reverse acc)

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
    split (TC.TyForall tcvars ([] TC.:=> ty)) = split ty
    split (TC.TyFun a b) =
      case collect [] b of
        (TC.TyCon newtypeName, conArgs) -> (newtypeName, \args -> subst (zip conArgs args) a)
        _ -> error "Weird newtype"
    split _ = error "Urk: Weird newtype"
    subst s ty =
      case lookup ty s of
        Just ty' -> ty'
        Nothing -> case ty of
          TC.TyApp a b -> TC.TyApp (subst s a) (subst s b)
          TC.TyFun a b -> TC.TyFun (subst s a) (subst s b)
          TC.TyTuple ls -> TC.TyTuple (map (subst s) ls)
          _ -> ty
    collect acc (TC.TyApp a b) = collect (b:acc) a
    collect acc ty = (ty, reverse acc)

-- XXX: Move this function.
isPrimitive :: Type -> Bool
isPrimitive Primitive{} = True
isPrimitive _ = False

convertDecl :: Core.Decl -> M ()
convertDecl (Core.Decl ty name (Lam vars expr)) = do
  vars' <- mapM convertVariable vars
  body <- local (\env -> env{envRoot = name}) $
          convertExpr False expr (pure . Return)
  retTys <- convertResultType vars' ty
  let fn = Function
          { fnName = name
          , fnAttributes = []
          , fnArguments = vars'
          , fnResults = retTys
          , fnBody = if name == entrypointName
                      then setProgramExit body
                      else body
          }
  tell [fn]
-- convertDecl (Core.Decl _ty name (WithCoercion _ expr)) =
--   convertDecl (Core.Decl _ty name expr)
convertDecl (Core.Decl ty name expr) = do
  body <- local (\env -> env{envRoot = name}) $
          convertExpr False expr (pure . Return)
  retTys <- convertResultType [] ty
  let fn = Function
          { fnName = name
          , fnAttributes = []
          , fnArguments = []
          , fnResults = retTys
          , fnBody = if name == entrypointName
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
            WithProof _proof e -> worker acc e
            -- WithProof proof e -> (e, Just proof, acc)
            -- WithCoercion c e -> (e, c, acc)
            _ -> (expr, Nothing, acc)

convertExpr :: Bool -> Core.Expr -> ([Variable] -> M Bedrock.Block) -> M Bedrock.Block
convertExpr lazy expr rest =
  case expr of
    UnboxedTuple args -> do
      tmp <- newVariable [] "unboxed" Node
      convertExprs args rest
    _ | (Con con, coercion, args) <- collectApps expr ->
      convertExprs args $ \args' -> do
        tmp <- newVariable [] "con" NodePtr
        let name = Core.varName con
        Just arity <- lookupArity name
        Bind [tmp] (Store (ConstructorName name (arity-length args)) args')
            <$> rest [tmp]
    _ | (Var v, coercion, args) <- collectApps expr, lazy ->
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
    _ | (Var v, coercion, args) <- collectApps expr, not lazy ->
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
        tmp <- newVariable [] "lit" (Primitive (CPointer I8))
        Bind [tmp] (Bedrock.Literal (LiteralString str))
            <$> rest [tmp]
    Core.Lit (Core.LitInt int) -> do
        tmp <- newVariable [] "int" (Primitive I64)
        Bind [tmp] (Bedrock.Literal (LiteralInt int))
            <$> rest [tmp]
    Core.Lit Core.LitVoid -> do
        tmp <- newVariable [] "void" (Primitive CVoid)
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
    Core.Case scrut var Nothing [Core.Alt (Core.UnboxedPat binds) branch]  ->
      convertExpr False scrut $ \vals -> do
        binds' <- mapM convertVariable binds
        let worker [] [] = convertExpr False branch rest
            worker (x:xs) (y:ys) = Bind [x] (TypeCast y) <$> worker xs ys
            worker [] _ = error "Not enough binds"
            worker _ [] = error "Not enough vals"
        worker binds' vals
    Core.Case scrut var Nothing alts | not lazy ->
      convertExpr False scrut $ \[val] ->
        if variableType val == NodePtr
          then do
            var' <- convertVariable var
            tmp <- deriveVariable val "node" Node
            -- Bind [var'] (Bedrock.TypeCast val) <$>
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
    Cast e ty ->
      convertExpr False e $ \[val] -> do
        var <- deriveVariable val "val" (convertTcType ty)
        Bind [var] (Bedrock.TypeCast val) <$> rest [var]
    WithExternal binder retS external args st scoped ->
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

    LetStrict name e1 e2 ->
      convertExpr False e1 $ \[val] -> do
          name' <- convertVariable name
          Bind [name'] (TypeCast val) <$> convertExpr lazy e2 rest

    WithProof _proof e -> convertExpr lazy e rest

    -- _ | lazy -> do
    --   body <- convertExpr False expr (pure . Return)
    --   (node, nodeArgs) <- pushFunction [] (Left "thunk") body
    --   tmp <- newVariable [] "thunk" NodePtr
    --   Bind [tmp] (Store (FunctionName node 0) nodeArgs)
    --     <$> rest [tmp]
    _ | lazy -> error $ "C->B convertExpr: " ++ show (lazy, expr)
    _ -> -- not lazy
      convertExpr True expr $ \[val] -> do
      tmp <- deriveVariable val "eval" NodePtr
      Bind [tmp] (Eval val)
          <$> rest [tmp]

convertExprs :: [Core.Expr] -> ([Variable] -> M Bedrock.Block) -> M Bedrock.Block
convertExprs [] fn = fn []
convertExprs (x:xs) fn =
    convertExpr True x $ \v ->
    convertExprs xs $ \vs ->
    fn (v ++ vs)

applyMany :: [Type] -> Variable -> [Variable] -> ([Variable] -> M Block) -> M Block
applyMany fianlTy node [] fn = fn [node]
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
            let name = Core.varName con
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
        LitChar c -> LiteralInt (fromIntegral $ ord c)
        LitString s -> LiteralString s
        LitInt i -> LiteralInt i
        LitVoid  -> LiteralInt 0
        _ -> error "Urk: literals"
        -- LitWord Integer
        -- LitFloat Rational
        -- LitDouble Rational

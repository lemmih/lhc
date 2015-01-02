{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Compiler.CoreToBedrock where


import           Compiler.Core hiding (Variable(..),NodeDefinition(..),UnboxedPat)
import qualified Compiler.Core                   as Core
import           Data.Bedrock                    as Bedrock
import           Data.Bedrock.Misc

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.RWS
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Char (ord)

import Language.Haskell.TypeCheck.Types (TcType(..))
import           Language.Haskell.TypeCheck.Monad (mkBuiltIn)

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
    (fns, ns) = runM (convertModule m) (Core.coreNamespace m)

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
                        WithCoercion _ (Lam vars _) -> length vars
                        _ -> 0 ] ++
        [ (name, length args)
        | Core.NodeDefinition name args <- coreNodes m ] ++
        [ (con, 1)
        | Core.NewType con <- coreNewTypes m ]

data Env = Env
    { envScope :: [Variable]
    , envArity :: Map Name Int
    , envRoot  :: Name
    , envLocation :: [String]
    }
newtype M a = M { unM :: RWS Env [Function] AvailableNamespace a }
    deriving
        ( Monad, MonadWriter [Function], MonadReader Env
        , MonadState AvailableNamespace
        , Applicative, Functor )
runM ::  M a -> AvailableNamespace -> ([Function], AvailableNamespace)
runM action ns = (fns, ns')
  where
    (ns',fns) = execRWS (unM action) env ns
    env = Env
        { envScope    = []
        , envArity    = Map.empty
        , envRoot     = error "envRoot"
        , envLocation = []
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

pushFunction :: Either String Name -> Bedrock.Block -> M Name
pushFunction origin body = do
    scope <- asks envScope
    Name orig ident _ <- asks envRoot
    name <- case origin of
                Left tag -> newName (orig++[ident]) tag
                Right n -> return n
    let fn = Function
            { fnName = name
            , fnAttributes = []
            , fnArguments = scope
            , fnResults = [Node]
            , fnBody = body }
    tell [fn]
    return name

-- XXX: Lookup the kind to find the bedrock type.
convertVariable :: Core.Variable -> M Variable
convertVariable var = return $
    Variable (Core.varName var) (convertTcType (Core.varType var))

convertTcType :: TcType -> Type
convertTcType tcTy =
    case tcTy of
        TcApp (TcCon qname) sub
            | qname == mkBuiltIn "LHC.Prim" "Addr"
                -> case convertTcType sub of
                     Primitive prim -> Primitive (CPointer prim)
                     _ -> error $ "CoreToBedrock: Addr must be primitive: " ++ show sub
        TcCon qname
            | qname == mkBuiltIn "LHC.Prim" "I8"
                 -> Primitive I8
        TcCon qname
            | qname == mkBuiltIn "LHC.Prim" "I32"
                 -> Primitive I32
        TcCon qname
            | qname == mkBuiltIn "LHC.Prim" "I64"
                 -> Primitive I64
        TcCon qname
            | qname == mkBuiltIn "LHC.Prim" "RealWorld#"
             -> Primitive I64
        TcFun{} -> NodePtr
        TcRef{} -> NodePtr
        TcCon{} -> NodePtr
        TcApp{} -> NodePtr
        TcUndefined -> NodePtr
        _ -> NodePtr
        _ -> error $ "CoreToBedrock: Unknown type: " ++ show tcTy

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
        InvokeHandler{} -> error "setProgramExit"
        Exit -> Exit
        Panic msg -> Panic msg

convertResultType :: TcType -> Type
convertResultType tcTy =
    case tcTy of
        TcApp (TcCon qname) sub
            | qname == mkBuiltIn "LHC.Prim" "Addr"
                -> case convertTcType sub of
                     Primitive prim -> Primitive (CPointer prim)
                     _ -> error $ "CoreToBedrock: Addr must be primitive: " ++ show sub
        TcCon qname
            | qname == mkBuiltIn "LHC.Prim" "I8"
                 -> Primitive I8
        TcCon qname
            | qname == mkBuiltIn "LHC.Prim" "I32"
                 -> Primitive I32
        TcCon qname
            | qname == mkBuiltIn "LHC.Prim" "I64"
                 -> Primitive I64
        TcCon qname
            | qname == mkBuiltIn "LHC.Prim" "RealWorld#"
                 -> Primitive I64
        TcFun _ a -> convertResultType a
        _ -> Node

-- XXX: Move this function.
isPrimitive :: Type -> Bool
isPrimitive Primitive{} = True
isPrimitive _ = False

convertDecl :: Core.Decl -> M ()
convertDecl (Core.Decl ty name (Lam vars expr)) = do
    vars' <- mapM convertVariable vars
    body <- local (\env -> env{envRoot = name}) $
            bind vars' $ convertExpr False expr (pure . Return)
    let fn = Function
            { fnName = name
            , fnAttributes = []
            , fnArguments = vars'
            , fnResults = [convertResultType ty]
            , fnBody = if name == entrypointName
                        then setProgramExit body
                        else body
            }
    tell [fn]
convertDecl (Core.Decl _ty name (WithCoercion _ expr)) =
    convertDecl (Core.Decl _ty name expr)
convertDecl (Core.Decl ty name expr) = do
    body <- local (\env -> env{envRoot = name}) $
            convertExpr False expr (pure . Return)
    let fn = Function
            { fnName = name
            , fnAttributes = []
            , fnArguments = []
            , fnResults = [convertResultType ty]
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

bind :: [Variable] -> M a -> M a
bind vs = local $ \env -> env{ envScope = envScope env ++ vs }

setOrigin :: M a -> M a
setOrigin = local $ \env ->
    let Name orig ident _ = envRoot env
    in env { envLocation = orig ++ [ident] }

collectApps :: Core.Expr -> (Core.Expr, [Core.Expr])
collectApps = worker []
  where
    worker acc expr =
        case expr of
            App a b -> worker (b:acc) a
            WithCoercion _ e -> worker acc e
            _ -> (expr, acc)

convertExpr :: Bool -> Core.Expr -> ([Variable] -> M Bedrock.Block) -> M Bedrock.Block
convertExpr lazy expr rest =
    case expr of
        UnboxedTuple args -> do
            tmp <- newVariable [] "unboxed" Node
            args' <- mapM convertVariable args
            Bind [tmp] (MkNode UnboxedTupleName args')
                <$> rest [tmp]
        WithCoercion _ e -> convertExpr lazy e rest
        --CaseUnboxed scrut binds branch | not lazy ->
        --    convertExpr False scrut $ \vars ->
        _ | (Con name, args) <- collectApps expr ->
            convertExprs args $ \args' -> do
                let ty = if lazy then NodePtr else Node
                    con = if lazy then Store else MkNode
                tmp <- newVariable [] "con" ty
                Just arity <- lookupArity name
                Bind [tmp] (con (ConstructorName name (arity-length args)) args')
                    <$> rest [tmp]
        --Var v | lazy -> do
        --    v' <- convertVariable v
        --    let fn = variableName v'
        --    mbArity <- lookupArity fn
        --    case mbArity of
        --        Nothing -> rest [v']
        --        Just arity -> do
        --            tmp <- newVariable [] "thunk" NodePtr
        --            Bind [tmp] (Store (FunctionName fn arity) [])
        --                <$> rest [tmp]
        _ | (Var v, args) <- collectApps expr, lazy ->
            convertExprs args $ \args' -> do
                v' <- convertVariable v
                let fn = variableName v'
                    fnRetTy = convertResultType (Core.varType v)
                mbArity <- lookupArity fn
                case mbArity of
                    Just arity | arity == length args' && isPrimitive fnRetTy -> do
                        tmp <- newVariable [] "thunk" fnRetTy
                        Bind [tmp] (Application fn args')
                            <$> rest [tmp]
                    Just arity | arity >= length args' -> do
                        tmp <- newVariable [] "susp" NodePtr
                        Bind [tmp] (Store (FunctionName fn (arity-length args')) args')
                            <$> rest [tmp]
                    -- Just arity | arity < length args' -> do
                    Nothing | null args -> do
                        rest [v']
                    Nothing -> do
                        body <- do
                            tmp <- deriveVariable v' "eval" Node
                            Bind [tmp] (Eval v')
                                <$> applyMany tmp args' (\next -> pure $ Return [next])
                        scope <- asks envScope
                        node <- pushFunction (Left "ap") body
                        tmp <- newVariable [] "ap" NodePtr
                        Bind [tmp] (Store (FunctionName node 0) scope)
                            <$> rest [tmp]
        _ | (Var v, args) <- collectApps expr, not lazy ->
            convertExprs args $ \args' -> do
                v' <- convertVariable v
                let fn = variableName v'
                    fnRetTy = convertResultType (Core.varType v)
                mbArity <- lookupArity fn
                case mbArity of
                    Nothing | null args ->
                        if variableType v' == NodePtr
                            then do
                                tmp <- deriveVariable v' "eval" Node
                                Bind [tmp] (Eval v')
                                    <$> rest [tmp]
                            else do
                                rest [v']
                    Just arity | arity == length args' -> do
                        tmp <- newVariable [] "thunk" fnRetTy
                        Bind [tmp] (Application fn args')
                            <$> rest [tmp]
                    Just arity | arity < length args' -> do
                        tmp <- newVariable [] "thunk" Node
                        Bind [tmp] (Application fn (take arity args'))
                            <$> applyMany tmp (drop arity args') (\next -> rest [next])
                    Just arity -> do
                        tmp <- newVariable [] "thunk" Node
                        Bind [tmp] (MkNode (FunctionName fn (arity-length args')) args')
                            <$> rest [tmp]
        -- Var v | not lazy -> do
        --     v' <- convertVariable v
        --     let fn = variableName v'
        --     mbArity <- lookupArity fn
        --     case mbArity of
        --         Nothing -> do
        --             tmp <- deriveVariable v' "eval" Node
        --             Bind [tmp] (Eval v')
        --                 <$> rest [tmp]
        --         Just arity -> do
        --             tmp <- newVariable [] "thunk" Node
        --             Bind [tmp] (MkNode (FunctionName fn arity) [])
        --                 <$> rest [tmp]
        Core.Lit (Core.LitString str) -> do
            tmp <- newVariable [] "lit" (Primitive (CPointer I8))
            Bind [tmp] (Bedrock.Literal (LiteralString str))
                <$> rest [tmp]
        Core.Lit (Core.LitInt int) -> do
            tmp <- newVariable [] "int" (Primitive I64)
            Bind [tmp] (Bedrock.Literal (LiteralInt int))
                <$> rest [tmp]
        App a b | lazy -> do
            body <- --setOrigin $
                convertExpr False a $ \[aVal] ->
                convertExpr True b $ \[bVal] ->  do
                ret <- deriveVariable aVal "apply" Node
                Bind [ret] (Apply aVal bVal)
                    <$> pure (Return [ret])
            scope <- asks envScope
            node <- pushFunction (Left "ap") body
            tmp <- newVariable [] "ap" NodePtr
            Bind [tmp] (Store (FunctionName node 0) scope)
                <$> rest [tmp]
        App a b | not lazy ->
            convertExpr False a $ \[aVal] -> -- node
            convertExpr True b $ \[bVal] -> do -- nodeptr
            ret <- deriveVariable aVal "apply" Node
            Bind [ret] (Apply aVal bVal)
                    <$> rest [ret]
        Lam v sub | lazy -> do
            scope <- asks envScope
            v' <- mapM convertVariable v
            body <- bind v' $ convertExpr False sub (pure . Return)
            node <- bind v' $ pushFunction (Left "lambda") body
            tmp <- newVariable [] "tmp" NodePtr
            Bind [tmp] (Store (FunctionName node (length v)) scope)
                <$> rest [tmp]
        --Lam v sub | not lazy -> do
        --    scope <- asks envScope
        --    body <- bind v $ convertExpr True sub (\val -> pure $ Return [val])
        --    node <- bind v $ pushFunction (Left "lam") body
        --    let tmp = Variable (Name [] "tmp" 0) Node
        --    Bind [tmp] (MkNode (FunctionName node (length v)) scope)
        --        <$> rest tmp
        Core.Case scrut var Nothing alts | not lazy ->
            convertExpr False scrut $ \[val] -> do
                var' <- convertVariable var
                -- Bind [var'] (Bedrock.TypeCast val) <$>
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
                    else
                        Bedrock.Case val (Just defExpr) <$>
                            mapM convertAlt alts
        --External external CVoid args st | not lazy -> do
        --    let tmp = Variable (Name [] "tmp" 0) NodePtr
        --        ret = Variable (Name [] "ret" 0) Node
        --        unit = ConstructorName (Name ["Main"] "IOUnit" 0)
        --        tuple = ConstructorName (Name ["Internal"] "()" 0)
        --    (Bind [] (CCall external args) .
        --     Bind [tmp] (Store tuple []) .
        --     Bind [ret] (MkNode unit [tmp, st]))
        --        <$> rest ret
        WithExternal binder external args st scoped | not lazy -> do
            binder' <- convertVariable binder
            args' <- mapM convertVariable args
            bind [binder'] $ Bind [binder'] (CCall external args')
                <$> convertExpr False scoped rest

        ExternalPure binder external args scoped | not lazy -> do
            binder' <- convertVariable binder
            args' <- mapM convertVariable args
            bind [binder'] $ Bind [binder'] (CCall external args')
                <$> convertExpr False scoped rest

        Let (NonRec name e1) e2 ->
            convertExpr True e1 $ \[val] -> do
                name' <- convertVariable name
                Bind [name'] (TypeCast val) <$> convertExpr lazy e2 rest

        LetStrict name e1 e2 ->
            convertExpr False e1 $ \[val] -> do
                name' <- convertVariable name
                Bind [name'] (TypeCast val) <$> convertExpr lazy e2 rest



        _ | lazy -> error $ "convertExpr: " ++ show (lazy, expr)
        _ | not lazy ->
            convertExpr True expr $ \[val] -> do
            tmp <- deriveVariable val "eval" Node
            Bind [tmp] (Eval val)
                <$> rest [tmp]

convertExprs :: [Core.Expr] -> ([Variable] -> M Bedrock.Block) -> M Bedrock.Block
convertExprs [] fn = fn []
convertExprs (x:xs) fn =
    convertExpr True x $ \v ->
    convertExprs xs $ \vs ->
    fn (v ++ vs)

applyMany :: Variable -> [Variable] -> (Variable -> M Block) -> M Block
applyMany node [] fn = fn node
applyMany node (x:xs) fn = do
    ret <- deriveVariable node "apply" Node
    Bind [ret] (Apply node x)
        <$> applyMany ret xs fn

deriveVariable :: Variable -> String -> Type -> M Variable
deriveVariable (Variable (Name orig ident _) _) tag ty = do
    path <- asks envLocation
    name <- newName (path ++ orig ++ [ident]) tag
    return $ Variable name ty

convertAlt :: Alt -> M Bedrock.Alternative
convertAlt (Alt pattern branch) =
    case pattern of
        ConPat name args -> do
            args' <- mapM convertVariable args
            bind args' $
              Alternative (NodePat (ConstructorName name 0) args')
                <$> convertExpr False branch (pure . Return)
        Core.LitPat lit ->
            Alternative
                <$> (Bedrock.LitPat <$> convertLiteral lit)
                <*> convertExpr False branch (pure . Return)
        Core.UnboxedPat args -> do
            args' <- mapM convertVariable args
            bind args' $
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
        -- LitWord Integer
        -- LitFloat Rational
        -- LitDouble Rational

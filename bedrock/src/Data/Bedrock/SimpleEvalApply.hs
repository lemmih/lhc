module Data.Bedrock.SimpleEvalApply (lowerEvalApply) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map               as Map
import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Data.Bedrock
import           Data.Bedrock.Transform

blackholeName :: Name
blackholeName = Name ["internal"] "BlackHole" 0

{-
Create eval function.
Create apply function for each argument/result pair.

eval *susp =
  obj <- fetch susp
  case obj of
    Function a b -> do
      ptr <- function a b
      update susp (Indirection ptr)
      return ptr
    _ ->
      return susp
    Function a _ ->
      return susp
    Cons a b ->
      return susp

apply *fn ?arg =
  obj <- fetch fn
  case obj of
    Function1 a _ -> function a ?arg
    Function2 _ _ -> store (Function2 ?arg _)
    Cons a _      -> store (Cons a ?arg)
-}
lowerEvalApply :: Gen ()
lowerEvalApply = do
  entrypoint <- gets (entryPoint . envModule)
  fs <- gets (Map.elems . envFunctions)
  evalName <- newName "eval"
  arg <- newVariable "arg" NodePtr
  let defaultEval = Return [arg]

  pushNode $ NodeDefinition blackholeName []

  evalBody <- do
    obj <- newVariable "obj" Node
    ind <- newVariable "ind" IWord
    target <- newVariable "target" NodePtr
    nonIndirection <-
      Bind [obj] (Fetch arg) <$>
        Case obj (Just defaultEval) <$> sequence
          [ mkEvalAlternative arg fn
          | fn <- fs
          , fnName fn /= entrypoint
          , fnResults fn == [NodePtr] ]
    --Bind [header] (Load arg 0) <$>
    pure $ Bind [ind] (Builtin "isIndirection" [PVariable arg]) $
      Case ind (Just nonIndirection) [Alternative (LitPat (LiteralInt 1)) $
        Bind [target] (Builtin "getIndirection" [PVariable arg]) $
        -- Return [target]
        TailCall evalName [target]
      ]

  pushFunction Function
    { fnName       = evalName
    , fnAttributes = []
    , fnArguments  = [arg]
    , fnResults    = [NodePtr]
    , fnBody       = evalBody}

  pairs <- applyPairs
  applys <- forM pairs $ \(ty, names) -> do
    appName <- mkApplyFn (ty, names)
    return (ty, appName)

  forM_ fs $ \fn ->
    pushFunction fn{fnBody = replaceEvalApply (Map.fromList applys) evalName (fnBody fn)}

mkApplyFn :: (([Type], Type), [(NodeName, [Type])]) -> Gen Name
mkApplyFn ((retTys, argTy), nodeNames) = do
    applyName <- newName "apply"
    fn <- newVariable "fn" NodePtr
    arg <- newVariable "arg" argTy
    obj <- newVariable "obj" Node
    applyBody <-
      Bind [obj] (Fetch fn) <$>
      Case obj Nothing <$> sequence
        [ do
            out <- mapM (newVariable "out") retTys
            args <- mapM (newVariable "arg") argTypes
            return $
              Alternative (NodePat name args) $
              Bind out (worker name (args ++ [arg])) $
              Return out
        | (name, argTypes) <- nodeNames ]
    pushFunction Function
      { fnName       = applyName
      , fnAttributes = []
      , fnArguments  = [fn, arg]
      , fnResults    = retTys
      , fnBody       = applyBody }
    return applyName
  where
    worker (ConstructorName con n) args = Store (ConstructorName con (n-1)) args
    worker (FunctionName fn 1) args = Application fn args
    worker (FunctionName fn n) args = Store (FunctionName fn (n-1)) args
    worker UnboxedTupleName _ = error "Urk: UnboxedTupleName"

applyPairs :: Gen [(([Type], Type), [(NodeName, [Type])])]
applyPairs = do
  fs <- gets (Map.elems . envFunctions)
  ns <- gets (nodes . envModule)
  let activeNodes = Set.unions (map functionNodes fs)
  return $ Map.toList $ Map.fromListWith (++) $
    [ ((retTys, argTy), [(nodeName, map variableType appliedArgs)])
    | fn <- fs
    , n <- [1..length (fnArguments fn)]
    , let nodeName = FunctionName (fnName fn) n
    , fnName fn `Set.member` activeNodes
    , let retTys = if n == 1 then fnResults fn else [NodePtr]
    , let appliedArgs = reverse (drop n (reverse (fnArguments fn)))
    , let argTy = variableType (reverse (fnArguments fn) !! (n-1)) ] ++
    [ ((retTys, argTy), [(nodeName, appliedArgs)])
    | NodeDefinition name args <- ns
    , n <- [1..length args]
    , let nodeName = ConstructorName name n
    , name `Set.member` activeNodes
    , let appliedArgs = reverse (drop n (reverse args))
    , let retTys = [NodePtr]
    , let argTy = reverse args !! (n-1) ]

functionNodes :: Function -> Set Name
functionNodes = blockNodes . fnBody

blockNodes :: Block -> Set Name
blockNodes block =
  case block of
    Case _scrut mbBlock alts ->
      maybe Set.empty blockNodes mbBlock `Set.union`
      Set.unions
        [ blockNodes branch
        | Alternative _pattern branch <- alts ]
    Bind _vars (Store node _args) rest ->
      setNodeName node (blockNodes rest)
    Bind _vars (MkNode node _args) rest ->
      setNodeName node (blockNodes rest)
    Bind _vars _ rest -> blockNodes rest
    Recursive _ rest -> blockNodes rest
    _ -> Set.empty

setNodeName :: NodeName -> Set Name -> Set Name
setNodeName UnboxedTupleName        = id
setNodeName (ConstructorName con _) = Set.insert con
setNodeName (FunctionName con _)    = Set.insert con

mkEvalAlternative :: Variable -> Function -> Gen Alternative
mkEvalAlternative ptr fn = do
    tmp <- newVariable "tmp" (StaticNode 1)
    newVal <- newVariable "new" NodePtr
    args <- mapM (newVariable "arg" .variableType) (fnArguments fn)
    return $ Alternative (NodePat nodeName args) $
      Bind [tmp] (MkNode (ConstructorName blackholeName 0) []) $
      Bind [] (Write ptr 0 tmp) $
      Bind [newVal] (Application (fnName fn) args) $
      Bind [] (Builtin "setIndirection" [PVariable ptr, PVariable newVal]) $
      -- XXX: Should be an 'Update arg (Indirection new)' here.
      Return [newVal]
  where
    nodeName = FunctionName (fnName fn) 0

replaceEvalApply :: Map.Map ([Type],Type) Name -> Name -> Block -> Block
replaceEvalApply applys eval = fix $ \loop block ->
  case block of
    Case scrut mbBlock alts ->
      Case scrut (fmap loop mbBlock)
        [ Alternative pat (loop branch)
        | Alternative pat branch <- alts ]
    Bind vars expr rest -> Bind vars (worker (map variableType vars) expr) (loop rest)
    Recursive binds rest ->
      Recursive binds (loop rest)
    Return vars -> Return vars
    Raise var -> Raise var
    TailCall fn args -> TailCall fn args
    Invoke fn args -> Invoke fn args
    Exit -> Exit
    Panic msg -> Panic msg
  where
    worker expectedRetTypes expr =
      case expr of
        Eval arg -> Application eval [arg]
        Apply fn arg ->
          case Map.lookup (expectedRetTypes, variableType arg) applys of
            Nothing -> error $ "Missing apply function: " ++ show (expectedRetTypes, variableType arg)
            Just apply -> Application apply [fn, arg]
        _        -> expr

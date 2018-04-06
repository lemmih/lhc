module Data.Bedrock.SimpleEvalApply (lowerEvalApply) where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map               as Map
import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Data.Bedrock
import           Data.Bedrock.Transform

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
    fs <- gets (Map.elems . envFunctions)
    evalName <- newName "eval"
    arg <- newVariable "arg" NodePtr
    let defaultEval = Return [arg]

    evalBody <- do
      obj <- newVariable "obj" Node
      Bind [obj] (Fetch memAttr arg) <$>
        Case obj (Just defaultEval) <$> sequence
          [ mkEvalAlternative fn
          | fn <- fs
          , fnResults fn == [NodePtr] ]
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

memAttr :: MemAttributes
memAttr = MemAttributes False Nothing

mkApplyFn :: (([Type], Type), [(NodeName, [Type])]) -> Gen Name
mkApplyFn ((retTys, arg), nodeNames) = do
    applyName <- newName "apply"
    fn <- newVariable "fn" NodePtr
    arg <- newVariable "arg" arg
    obj <- newVariable "obj" Node
    applyBody <-
      Bind [obj] (Fetch memAttr fn) <$>
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
    , nodeName `Set.member` activeNodes
    , let retTys = if n == 1 then fnResults fn else [NodePtr]
    , let appliedArgs = reverse (drop n (reverse (fnArguments fn)))
    , let argTy = variableType (reverse (fnArguments fn) !! (n-1)) ] ++
    [ ((retTys, argTy), [(nodeName, appliedArgs)])
    | NodeDefinition name args <- ns
    , n <- [1..length args]
    , let nodeName = ConstructorName name n
    , nodeName `Set.member` activeNodes
    , let appliedArgs = reverse (drop n (reverse args))
    , let retTys = [NodePtr]
    , let argTy = reverse args !! (n-1) ]

functionNodes :: Function -> Set NodeName
functionNodes = blockNodes . fnBody

blockNodes :: Block -> Set NodeName
blockNodes block =
  case block of
    Case _scrut mbBlock alts ->
      maybe Set.empty blockNodes mbBlock `Set.union`
      Set.unions
        [ blockNodes branch
        | Alternative _pattern branch <- alts ]
    Bind _vars (Store node _args) rest ->
      Set.insert node (blockNodes rest)
    Bind _vars (MkNode node _args) rest ->
      Set.insert node (blockNodes rest)
    Bind _vars _ rest -> blockNodes rest
    _ -> Set.empty

mkEvalAlternative :: Function -> Gen Alternative
mkEvalAlternative fn = do
    newVal <- newVariable "new" NodePtr
    args <- mapM (newVariable "arg" .variableType) (fnArguments fn)
    return $ Alternative (NodePat nodeName args) $
      Bind [newVal] (Application (fnName fn) args) $
      -- XXX: Should be an 'Update arg (Indirection new)' here.
      Return [newVal]
  where
    nodeName = FunctionName (fnName fn) 0

replaceEvalApply :: Map.Map ([Type],Type) Name -> Name -> Block -> Block
replaceEvalApply applys eval = fix $ \loop block ->
  case block of
    Case scrut mbBlock alts ->
      Case scrut (fmap loop mbBlock)
        [ Alternative pattern (loop block)
        | Alternative pattern block <- alts ]
    Bind vars expr rest -> Bind vars (worker (map variableType vars) expr) (loop rest)
    Return vars -> Return vars
    Raise var -> Raise var
    TailCall fn args -> TailCall fn args
    Invoke fn args -> Invoke fn args
    InvokeHandler fn cont -> InvokeHandler fn cont
    Exit -> Exit
    Panic msg -> Panic msg
  where
    worker expectedRetTypes expr =
      case expr of
        Eval arg -> Application eval [arg]
        Apply fn arg ->
          case Map.lookup (expectedRetTypes, variableType arg) applys of
            Nothing -> Undefined -- error $ "Missing apply function: " ++ show (fn,arg)
            Just apply -> Application apply [fn, arg]
        _        -> expr

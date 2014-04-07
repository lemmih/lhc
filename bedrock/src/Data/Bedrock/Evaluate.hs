module Data.Bedrock.Evaluate where

import           Control.Applicative           ((<$>))
import           Control.Monad.RWS
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Text.ParserCombinators.Parsec (parseFromFile)
import qualified Text.PrettyPrint.ANSI.Leijen  as Doc
import Data.List

import           Data.Bedrock
import           Data.Bedrock.Exceptions
import           Data.Bedrock.Parse
import           Data.Bedrock.PrettyPrint

type HeapPtr = Int
data Value
    = LitValue Literal
    | NodeValue NodeName [Value]
    | HeapPtrValue HeapPtr
    deriving (Show)
data Env = Env
    { envFunctions :: Map Name Function
    , envHeap      :: Map HeapPtr Value
    , envHeapPtr   :: HeapPtr
    }
type Scope = Map Variable Value
type Eval a = RWST Scope () Env IO a

-- FIXME: Enforce that only nodes are written to the heap.
pushHeapValue :: Value -> Eval HeapPtr
pushHeapValue val = do
    ptr <- gets envHeapPtr
    modify $ \st -> st{ envHeapPtr = ptr+1
                      , envHeap = Map.insert ptr val (envHeap st) }
    return ptr

fetchHeapValue :: HeapPtr -> Eval Value
fetchHeapValue ptr = do
    mbValue <- gets (Map.lookup ptr . envHeap)
    case mbValue of
        Nothing  -> error "Broken heap ptr"
        Just val -> return val

queryScope :: Variable -> Eval Value
queryScope var = do
    mbValue <- asks (Map.lookup var)
    case mbValue of
        Nothing -> error $ "Not in scope: " ++ show var
        Just val -> return val

lookupFunction :: Name -> Eval Function
lookupFunction name = do
    mbFn <- gets (Map.lookup name . envFunctions)
    case mbFn of
        Nothing -> error $ "Missing function: " ++ show (ppName name)
        Just fn -> return fn



evaluateFromFile :: FilePath -> Name -> IO ()
evaluateFromFile path entryPoint = do
    ret <- parseFromFile parseModule path
    case ret of
        Left err -> print err
        Right m  -> do
            let m' = runGen m $ cpsTransformation m
            print (ppModule m')
            evaluate m' entryPoint


evaluate :: Module -> Name -> IO ()
evaluate m entryPoint = do
    _ <- evalRWST entry Map.empty env
    return ()
  where
    env = Env
        { envFunctions = Map.fromList [ (fnName fn, fn) | fn <- functions m ]
        , envHeap      = Map.empty
        , envHeapPtr   = 0 }
    entry = evalFunction entryPoint [LitValue $ LiteralInt 0]

evalFunction :: Name -> [Value] -> Eval [Value]
evalFunction name args = do
    fn <- lookupFunction name
    let newScope = Map.fromList $ zip (fnArguments fn) args
    local (Map.union newScope)
        (evalExpression (fnBody fn))

evalExpression :: Expression -> Eval [Value]
evalExpression expression =
    case expression of
        Case scrut alternatives _defaultBranch -> do
            value <- queryScope scrut
            evalAlternative value alternatives
        Bind binds simple rest -> do
            values <- evalSimple simple
            let newScope = Map.fromList $ zip binds values
            local (Map.union newScope) $
                evalExpression rest
        TailCall fn args ->
            evalFunction fn =<< mapM evalArgument args
        Exit ->
            return []
        Invoke cont args ->
            evalApply cont args
        other -> error $ "Unhandled code: " ++ show other

evalAlternative :: Value -> [Alternative] -> Eval [Value]
evalAlternative _value [] = error "No matching branches"
evalAlternative value (Alternative pattern branch:alts) =
    case (value, pattern) of
        (LitValue lit, LitPat litBranch) | lit == litBranch ->
            evalExpression branch
        (NodeValue name values, NodePat nameBranch binds) | name == nameBranch ->
            let newScope = Map.fromList $ zip binds values in
            local (Map.union newScope)
                (evalExpression branch)
        _ -> evalAlternative value alts

evalSimple :: SimpleExpression -> Eval [Value]
evalSimple simple =
    case simple of
        Literal lit -> return [LitValue lit]
        --Application fn args -> undefined
        Store name args -> do
            argValues <- mapM evalArgument args
            ptr <- pushHeapValue (NodeValue name argValues)
            return [HeapPtrValue ptr]
        Fetch var -> do
            HeapPtrValue ptr <- queryScope var
            value <- fetchHeapValue ptr
            return [value]
        Print var -> do
            value <- queryScope var
            liftIO $ putStr (show (ppVariable var) ++ " = ")
            trace <- traceValue value
            liftIO $ putStrLn trace
            return []
        _ -> error $ "Unhandled expr: " ++ show simple

evalArgument :: Argument -> Eval Value
evalArgument argument =
    case argument of
        RefArg var -> queryScope var
        LitArg lit -> return (LitValue lit)
        NodeArg name args -> NodeValue name <$> mapM evalArgument args

traceValue :: Value -> Eval String
traceValue value =
    case value of
        LitValue lit        -> return $ show lit
        NodeValue name args -> do
            args' <- mapM traceValue args
            return $ show $ ppNode name (map Doc.text args')
        HeapPtrValue ptr    -> do
            trace <- traceValue =<< fetchHeapValue ptr
            return $ "Heap (" ++ trace ++ ")"

evalApply :: Variable -> [Argument] -> Eval [Value]
evalApply continuationVar args = do
    args' <- mapM evalArgument args
    continuation <- queryScope continuationVar
    activateFunctionFrame continuation args'


data FrameDescription
    = FunctionFrame
    | ExceptionHandlerFrame Value
unpackFrame :: Value -> Eval (FrameDescription, (Name, Int, [Value]))
unpackFrame = loop
  where
    loop (HeapPtrValue ptr) = loop =<< fetchHeapValue ptr
    loop (NodeValue
            (ConstructorName name)
            [continuation, NodeValue (FunctionName handlerName blanks) args])
        | name == exhFrameName =
        return ( ExceptionHandlerFrame continuation
               , (handlerName, blanks, args))
    loop (NodeValue (FunctionName name blanks) args) =
        return (FunctionFrame, (name, blanks, args))

skipFrame :: (Name, Int, [Value]) -> Eval Value
skipFrame (name, _blanks, partialArgs) = do
    fn <- lookupFunction name
    case elemIndex stdContinuation (fnArguments fn) of
        Just index -> do
            liftIO $ putStrLn $ "Found normal frame: " ++ show (ppName name)
            return (partialArgs!!index)
        Nothing    -> do
            error "Invalid frame"

activateFrame :: (Name, Int, [Value]) -> [Value] -> Eval [Value]
activateFrame (name, blanks, partialArgs) args = do
    unless (blanks == length args) $ error "arity mismatch"
    evalFunction name (partialArgs ++ args)

activateFunctionFrame :: Value -> [Value] -> Eval [Value]
activateFunctionFrame frame args = do
    (descr, info) <- unpackFrame frame
    case descr of
        FunctionFrame -> activateFrame info args
        ExceptionHandlerFrame continuation ->
            activateFunctionFrame continuation args

activateExceptionFrame :: Value -> [Value] -> Eval [Value]
activateExceptionFrame frame args = do
    (descr, info) <- unpackFrame frame
    case descr of
        FunctionFrame ->
            flip activateExceptionFrame args =<< skipFrame info
        ExceptionHandlerFrame continuation ->
            activateFrame info (args ++ [continuation])

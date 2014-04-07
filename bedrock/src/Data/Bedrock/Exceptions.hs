module Data.Bedrock.Exceptions
    ( runGen
    , cpsTransformation
    , loadFile
    , stdContinuation
    , exhFrameName
    ) where

import           Control.Applicative           (pure, (<$>), (<*>))
import           Control.Monad.State
import           Data.List                     ((\\), elemIndices)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Map                     (Map)
import qualified Data.Map                      as Map
import           Text.ParserCombinators.Parsec (parseFromFile)

import           Data.Bedrock
import           Data.Bedrock.Parse
import           Data.Bedrock.PrettyPrint


data Env = Env
    { envModule    :: Module
    , envUnique    :: Int
    , envFunctions :: Map Name Function }
type Gen a = State Env a

modifyModule :: (Module -> Module) -> Gen ()
modifyModule fn = modify $ \st -> st{envModule = fn (envModule st)}

pushFunction :: Function -> Gen ()
pushFunction fn = modifyModule $ \m -> m{functions = functions m ++ [fn]}

newUnique :: Gen Int
newUnique = do
    u <- gets envUnique
    modify $ \st -> st{envUnique = u+1}
    return u

-- FIXME: This is O(n)
--lookupFunction :: Name -> Gen Function
--lookupFunction name = do
--    funcs <- gets envFunctions
--    case Map.lookup name funcs of
--        Just fn -> return fn
--        Nothing -> error $ "Missing function: " ++ show name

runGen :: Module -> Gen a -> Module
runGen initModule gen =
    envModule (execState gen m)
  where
    m = Env
        { envModule = Module [] []
        , envUnique = 0
        , envFunctions = Map.fromList
            [ (fnName fn, fn) | fn <- functions initModule]
        }


cpsTransformation :: Module -> Gen ()
cpsTransformation m = do
    mapM_ cpsFunction (functions m)
    mkThrowTo

mkThrowTo :: Gen ()
mkThrowTo = do
    fns <- gets (functions . envModule)

    let thisContinuationPtr = Variable (Name [] "thisContPtr" 0) NodePtr
        thisContinuation = Variable (Name [] "thisCont" 0) RawNode
        exception = Variable (Name [] "exception" 0) NodePtr
        handler = Variable (Name [] "handler" 0) RawNode
        body =
            Bind [thisContinuation] (Fetch thisContinuationPtr) $
            Case thisContinuation (exhAlternative:alternatives) Nothing 
        exhAlternative =
            Alternative
                (NodePat
                    (ConstructorName exhFrameName)
                    [thisContinuationPtr, handler])
                (Invoke handler [RefArg exception, RefArg thisContinuationPtr])
        alternatives =
            [ Alternative
                (NodePat
                    (FunctionName (fnName fn) blanks)
                    (reverse . drop blanks . reverse $ fnArguments fn))
                (TailCall throwToName
                    (map RefArg [fnArguments fn !! idx, exception]))
            | fn <- fns
            , idx <- elemIndices stdContinuation (fnArguments fn)
            , blanks <- [0 .. length (fnArguments fn) - 1 - idx] ]
    pushFunction Function
        { fnName = throwToName
        , fnArguments = [thisContinuationPtr, exception]
        , fnBody = body }

throwToName :: Name
throwToName = Name [] "throwTo" 0

cpsFunction :: Function -> Gen ()
cpsFunction fn = do
    body <- cpsExpression fn (fnBody fn)
    let fn' = fn{fnArguments = fnArguments fn ++ [stdContinuation]
                ,fnBody = body}
    pushFunction fn'

cpsExpression :: Function -> Expression -> Gen Expression
cpsExpression origin expression =
    case expression of
        Bind binds simple rest ->
            cpsSimpleExpresion origin binds simple =<<
                cpsExpression origin rest
        Return args ->
            return $ Invoke stdContinuation args
        Case scrut alternatives defaultBranch ->
            Case scrut
                <$> mapM (cpsAlternative origin) alternatives
                <*> pure defaultBranch
        Throw exception ->
            --return $ ThrowTo stdContinuation exception
            return $ TailCall throwToName [RefArg stdContinuation, exception]
        other -> return other

tagName :: String -> Name -> Gen Name
tagName tag name = do
    u <- newUnique
    return $ name{ nameIdentifier = nameIdentifier name ++ "_" ++ tag
                 , nameUnique = u}

exhFrameName :: Name
exhFrameName = Name [] "ExceptionFrame" 0

cpsSimpleExpresion :: Function -> [Variable]
                   -> SimpleExpression -> Expression -> Gen Expression
cpsSimpleExpresion origin binds simple rest =
    case simple of
        WithExceptionHandler exh exhArgs fn fnArgs -> do
            exFrameName <- tagName ("exception_frame") (fnName origin)
            let exceptionFrame = Variable
                    { variableName = exFrameName
                    , variableType = NodePtr }

            -- We have to create an indirection to shuffle
            -- around the arguments to the exception handler.
            -- This indirection can be removed later.
            --indirectName <- tagName "indirect" exh
            --exhFn <- lookupFunction exh
            --pushFunction Function
            --    { fnName      = indirectName
            --    , fnArguments = init (fnArguments exhFn) ++
            --                    [stdContinuation, last (fnArguments exhFn)]
            --    , fnBody      = TailCall exh
            --        (map RefArg $ fnArguments exhFn ++ [stdContinuation])
            --    }
            mkContinuation $ \continuationFrame ->
                Bind [exceptionFrame]
                    (Store (ConstructorName exhFrameName)
                        [ continuationFrame
                        , NodeArg (FunctionName exh 2) exhArgs ]) $
                TailCall fn (fnArgs ++ [RefArg exceptionFrame])
        Application fn fnArgs ->
            mkContinuation $ \continuationFrame ->
                TailCall fn (fnArgs ++ [continuationFrame])
        Store (FunctionName fn blanks) args ->
            return $ Bind binds (Store (FunctionName fn (blanks+1)) args) rest
        other -> return $ Bind binds other rest
  where    
    mkContinuation use = do
        cFrameName <- tagName ("frame") (fnName origin)
        let stdContinuationFrame = Variable
                { variableName = cFrameName
                , variableType = NodePtr }
    
        let continuationArgs = (Set.toList (freeVariables rest) \\ binds)
        contFnName <- tagName "continuation" (fnName origin)
        pushFunction $
            Function { fnName      = contFnName
                     , fnArguments = continuationArgs ++ binds
                     , fnBody      = rest }
        return $
            Bind [stdContinuationFrame]
                (Store (FunctionName contFnName (length binds))
                    (map RefArg continuationArgs)) $
            use (RefArg stdContinuationFrame)

cpsAlternative :: Function -> Alternative -> Gen Alternative
cpsAlternative origin alternative =
    case alternative of
        Alternative pattern expr ->
            Alternative pattern <$> cpsExpression origin expr

freeVariables :: Expression -> Set Variable
freeVariables expr = freeVariables' expr Set.empty

freeVariables' :: Expression -> Set Variable -> Set Variable
freeVariables' expression =
    case expression of
        Case scrut alternatives _defaultBranch ->
            foldr (.) (Set.insert scrut)
            [ freeVariables' branch
            | Alternative _pattern branch <- alternatives ]
        Bind binds simple rest ->
            freeVariablesSimple simple .
            flip Set.difference (Set.fromList binds) .
            freeVariables' rest
        Return args ->
            freeVariablesArguments args
        Throw name ->
            freeVariablesArguments [name]
        Invoke cont args ->
            Set.insert cont . freeVariablesArguments args
        TailCall _name args ->
            freeVariablesArguments args
        Exit ->
            id

freeVariablesSimple :: SimpleExpression -> Set Variable -> Set Variable
freeVariablesSimple simple =
    case simple of
        Literal{} ->
            id
        Application _fn args ->
            freeVariablesArguments args
        WithExceptionHandler _exh exhArgs _fn fnArgs ->
            freeVariablesArguments exhArgs . freeVariablesArguments fnArgs
        Alloc{} ->
            id
        Store _constructor args ->
            freeVariablesArguments args
        Fetch ptr ->
            Set.insert ptr
        Load ptr _idx ->
            Set.insert ptr
        Add lhs rhs ->
            freeVariablesArguments [lhs,rhs]
        Print var ->
            Set.insert var

freeVariablesArguments :: [Argument] -> Set Variable -> Set Variable
freeVariablesArguments args =
    Set.union (Set.fromList [ name | RefArg name <- args ])

stdContinuation :: Variable
stdContinuation = Variable (Name [] "cont" 0) NodePtr

loadFile :: FilePath -> IO ()
loadFile path = do
    ret <- parseFromFile parseModule path
    case ret of
        Left err -> print err
        Right m  -> print (ppModule $ runGen m $ cpsTransformation m)

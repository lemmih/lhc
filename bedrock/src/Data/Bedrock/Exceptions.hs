module Data.Bedrock.Exceptions
    ( runGen
    , cpsTransformation
    , loadFile
    , stdContinuation
    , isCatchFrame
    ) where

import           Control.Applicative           (pure, (<$>), (<*>))
import           Control.Monad.State
import           Data.List                     ((\\))
import qualified Data.Set                      as Set
import qualified Data.Map as Map
import           Text.ParserCombinators.Parsec (parseFromFile)

import           Data.Bedrock
import           Data.Bedrock.Parse
import           Data.Bedrock.PrettyPrint
import           Data.Bedrock.Transform


cpsTransformation :: Gen ()
cpsTransformation = do
    fs <- gets (Map.elems . envFunctions)
    mapM_ cpsFunction fs
    -- FIXME: Exception handling is broken.
    -- mkThrowTo

--_mkThrowTo :: Gen ()
--_mkThrowTo = do
--    fns <- gets (functions . envModule)

--    let nextContinuationPtr = Variable (Name [] "nextContPtr" 0) NodePtr
--        thisContinuation = Variable (Name [] "thisCont" 0) Node
--        exception = Variable (Name [] "exception" 0) NodePtr
--        handler = Variable (Name [] "handler" 0) Node
--        body =
--            Case thisContinuation Nothing (exhAlternative:alternatives)
--        exhAlternative =
--            Alternative
--                (NodePat
--                    (ConstructorName exhFrameName)
--                    [nextContinuationPtr, handler])
--                (Invoke handler [exception, nextContinuationPtr])
--        alternatives =
--            [ Alternative
--                (NodePat
--                    (FunctionName (fnName fn) blanks)
--                    (reverse . drop blanks . reverse $ fnArguments fn))
--                (TailCall throwToName
--                    [fnArguments fn !! idx, exception])
--            | fn <- fns
--            , idx <- elemIndices stdContinuation (fnArguments fn)
--            , blanks <- [0 .. length (fnArguments fn) - 1 - idx] ]
--    pushFunction Function
--        { fnName = throwToName
--        , fnArguments = [thisContinuation, exception]
--        , fnResults = []
--        , fnBody = body }

--throwToName :: Name
--throwToName = Name [] "throwTo" 0

cpsFunction :: Function -> Gen ()
cpsFunction fn = do
    body <- cpsBlock fn (fnBody fn)
    let fn' = fn{fnArguments = fnArguments fn ++ [stdContinuation]
                ,fnResults = []
                ,fnBody = body}
    pushFunction fn'

cpsBlock :: Function -> Block -> Gen Block
cpsBlock origin block =
    case block of
        Bind binds simple rest ->
            cpsExpresion origin binds simple =<<
                cpsBlock origin rest
        Return args -> do
            node <- newVariable "contNode" Node
            return $
                Bind [node] (Fetch stdContinuation) $
                Invoke node args
        Case scrut defaultBranch alternatives ->
            Case scrut
                <$> pure defaultBranch
                <*> mapM (cpsAlternative origin) alternatives
        Raise exception -> do
            node <- newVariable "contNode" Node
            return $
                Bind [node] (Fetch stdContinuation) $
                InvokeHandler node exception
        TailCall fn args ->
            pure $ TailCall fn (args ++ [stdContinuation])
        other -> return other

exhFrameIdentifier :: String
exhFrameIdentifier = "CatchFrame"

isCatchFrame :: Name -> Bool
isCatchFrame (Name [] ident _) = exhFrameIdentifier == ident
isCatchFrame _ = False

cpsExpresion :: Function -> [Variable]
             -> Expression -> Block -> Gen Block
cpsExpresion origin binds simple rest =
    case simple of
        Catch exh exhArgs fn fnArgs -> do
            exFrameName <- tagName ("exception_frame") (fnName origin)
            let exceptionFrame = Variable
                    { variableName = exFrameName
                    , variableType = FramePtr }
                exContPtr = Variable
                    { variableName = Name [] "exCont" 0
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
            -- FIXME:
            exhFrameName <- newName exhFrameIdentifier
            pushNode $ NodeDefinition exhFrameName [FramePtr, NodePtr]
            mkContinuation $ \continuationFrame ->
                -- FIXME: continuationFrame needs to be stored.
                Bind [exContPtr] (Store (FunctionName exh 2) exhArgs) $
                Bind [exceptionFrame]
                    (Store (ConstructorName exhFrameName)
                        [ continuationFrame
                        , exContPtr ]) $
                TailCall fn (fnArgs ++ [exceptionFrame])
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
                , variableType = FramePtr }
    
        let continuationArgs = (Set.toList (freeVariables rest) \\ binds)
        contFnName <- tagName "continuation" (fnName origin)
        pushFunction $
            Function { fnName      = contFnName
                     , fnArguments = continuationArgs ++ binds
                     , fnResults   = []
                     , fnBody      = rest }
        return $
            Bind [stdContinuationFrame]
                (Store (FunctionName contFnName (length binds))
                    continuationArgs) $
            use stdContinuationFrame

cpsAlternative :: Function -> Alternative -> Gen Alternative
cpsAlternative origin (Alternative pattern expr) =
    case pattern of
        NodePat (FunctionName fn n) args ->
            Alternative
                (NodePat (FunctionName fn (n+1)) args)
                <$> cpsBlock origin expr
        _ -> Alternative pattern <$> cpsBlock origin expr



stdContinuation :: Variable
stdContinuation = Variable (Name [] "cont" 0) FramePtr

loadFile :: FilePath -> IO ()
loadFile path = do
    ret <- parseFromFile parseModule path
    case ret of
        Left err -> print err
        Right m  -> print (ppModule $ runGen m $ cpsTransformation)

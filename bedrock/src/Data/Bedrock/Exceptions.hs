module Data.Bedrock.Exceptions
    ( runGen
    , cpsTransformation
    , stdContinuation
    , isCatchFrame
    ) where

import           Control.Applicative           (pure, (<$>), (<*>))
import           Control.Monad.State
import           Data.List                     ((\\))
import qualified Data.Set                      as Set
import qualified Data.Map as Map

import           Data.Bedrock
import           Data.Bedrock.Transform
import           Data.Bedrock.Misc (constantMemory)


cpsTransformation :: Gen ()
cpsTransformation = do
    fs <- gets (Map.elems . envFunctions)
    mapM_ cpsFunction fs

cpsFunction :: Function -> Gen ()
cpsFunction fn | NoCPS `elem` fnAttributes fn =
    pushFunction fn
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
                Bind [node] (Fetch constantMemory stdContinuation) $
                Invoke node args
        Case scrut defaultBranch alternatives ->
            Case scrut
                <$> pure defaultBranch
                <*> mapM (cpsAlternative origin) alternatives
        Raise exception -> do
            node <- newVariable "contNode" Node
            return $
                Bind [node] (Fetch constantMemory stdContinuation) $
                InvokeHandler node exception
        TailCall fn args -> do
            noCPS <- hasAttribute fn NoCPS
            if noCPS
                then pure $ TailCall fn args
                else pure $ TailCall fn (args ++ [stdContinuation])
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
                exSusp = Variable
                    { variableName = Name [] "exSusp" 0
                    , variableType = Node }
            exhFrameName <- newName exhFrameIdentifier
            pushNode $ NodeDefinition exhFrameName [FramePtr, Node]
            mkContinuation $ \continuationFrame ->
                Bind [exSusp] (MkNode (FunctionName exh 2) exhArgs) $
                Bind [exceptionFrame]
                    (Store (ConstructorName exhFrameName 0)
                        [ continuationFrame
                        , exSusp ]) $
                TailCall fn (fnArgs ++ [exceptionFrame])
        Application fn fnArgs -> do
            noCPS <- hasAttribute fn NoCPS
            if noCPS
                then return $ Bind binds (Application fn fnArgs) rest
                else mkContinuation $ \continuationFrame ->
                        TailCall fn (fnArgs ++ [continuationFrame])
        Store (FunctionName fn blanks) args ->
            return $ Bind binds (Store (FunctionName fn (blanks+1)) args) rest
        MkNode (FunctionName fn blanks) args ->
            return $ Bind binds (MkNode (FunctionName fn (blanks+1)) args) rest
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
                     , fnAttributes = []
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





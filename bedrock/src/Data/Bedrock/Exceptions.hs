module Data.Bedrock.Exceptions
    ( runGen
    , cpsTransformation
    , stdContinuation
    , isCatchFrame
    ) where

import           Control.Applicative           (pure, (<$>), (<*>))
import           Control.Monad.State
import qualified Data.Map as Map

import           Data.Bedrock
import           Data.Bedrock.Transform
import           Data.Bedrock.Misc (anyMemory)

stackFrameName :: Name
stackFrameName = Name ["bedrock"] "StackFrame" 0

cpsTransformation :: Gen ()
cpsTransformation = do
    pushNode $ NodeDefinition stackFrameName []
    fs <- gets (Map.elems . envFunctions)
    mapM_ cpsFunction fs

cpsFunction :: Function -> Gen ()
cpsFunction fn | NoCPS `elem` fnAttributes fn =
    pushFunction fn
cpsFunction fn = do
    frameVar <- newVariable "bedrock.stackframe" FramePtr
    body <- cpsBlock fn frameVar (fnBody fn)
    let size = frameSize (fnBody fn)
        bodyWithFrame =
            Bind [] (Alloc $ size+1) $ -- size + header
            Bind [frameVar] (Store (ConstructorName stackFrameName 0) []) $
            Bind [] (BumpHeapPtr size) $
            Bind [] (Write frameVar 2 stdContinuation) $
            body
    let fn' = fn{fnArguments = fnArguments fn ++ [stdContinuation]
                ,fnResults = []
                ,fnBody = if size > 0 then bodyWithFrame else body}
    pushFunction fn'


cpsBlock :: Function -> Variable -> Block -> Gen Block
cpsBlock origin frameVar block =
    case block of
        Bind binds simple rest ->
            cpsExpresion origin frameVar binds simple rest
        Return args -> do
            node <- newVariable "contNode" IWord
            let size = case fnResults origin of
                        [StaticNode n] -> n
                        _ -> length args
            -- We need to invoke with at least 1+'size' arguments.
            -- 'node' is repeated here. Could be Undefined as well. Doesn't matter.
            return $
                Bind [node] (Load anyMemory stdContinuation 1) $
                Invoke node (stdContinuation : take size (args ++ repeat node))
        Case scrut defaultBranch alternatives ->
            Case scrut
                <$> maybe (return Nothing) (fmap Just . cpsBlock origin frameVar) defaultBranch
                <*> mapM (cpsAlternative origin frameVar) alternatives
        Raise exception -> do
            node <- newVariable "contNode" Node
            return $
                Bind [node] (Fetch anyMemory stdContinuation) $
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

cpsExpresion :: Function -> Variable -> [Variable]
             -> Expression -> Block -> Gen Block
cpsExpresion origin frameVar binds simple rest =
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
            Bind binds (Store (FunctionName fn (blanks+1)) args)
                <$> cpsBlock origin frameVar rest
        MkNode (FunctionName fn blanks) args ->
            Bind binds (MkNode (FunctionName fn (blanks+1)) args)
                <$> cpsBlock origin frameVar rest
        Save var n ->
            Bind binds (Write frameVar n var) <$> cpsBlock origin frameVar rest
        Restore n ->
            Bind binds (Load anyMemory frameVar n) <$> cpsBlock origin frameVar rest
        other -> Bind binds other <$> cpsBlock origin frameVar rest
  where
    mkContinuation use = do

        fnPtr <- newVariable "fnPtr" (Primitive $ CPointer (CFunction CVoid [CPointer I8]))

        framePtr <- newVariable "bedrock.stackframe.cont" FramePtr

        contFnName <- tagName "continuation" (fnName origin)

        body <- cpsBlock origin framePtr $
            Bind [stdContinuation] (Load anyMemory framePtr 2) $ rest
        pushHelper $
            Function { fnName      = contFnName
                     , fnAttributes = []
                     , fnArguments = framePtr : binds
                     , fnResults   = []
                     , fnBody      = body }

        return $
            Bind [fnPtr] (FunctionPointer contFnName) $
            Bind [] (Write frameVar 1 fnPtr) $
            use frameVar

cpsAlternative :: Function -> Variable -> Alternative -> Gen Alternative
cpsAlternative origin frameVar (Alternative pattern expr) =
    case pattern of
        NodePat (FunctionName fn n) args ->
            Alternative
                (NodePat (FunctionName fn (n+1)) args)
                <$> cpsBlock origin frameVar expr
        _ -> Alternative pattern <$> cpsBlock origin frameVar expr



stdContinuation :: Variable
stdContinuation = Variable (Name [] "cont" 0) FramePtr

frameSize :: Block -> Int
frameSize block =
    case block of
        Bind _ Application{} rest -> max 2 (frameSize rest)
        Bind _ (Restore n) rest -> max n (frameSize rest)
        Bind _ _ rest -> frameSize rest
        Return{} -> 0
        Case _scrut defaultBranch alts -> maximum
            ((case defaultBranch of
                Nothing -> 0
                Just branch -> frameSize branch) :
            [ frameSize branch | Alternative _ branch <- alts ])
        TailCall{} -> 0
        Exit -> 0
        _ -> error "Data.Bedrock.Exceptions.frameSize"

module Data.Bedrock.Exceptions
    ( runGen
    , cpsTransformation
    , stdContinuation
    , isCatchFrame
    ) where

import           Control.Applicative    (pure, (<$>), (<*>))
import           Control.Monad.State
import qualified Data.Map               as Map
-- import           Data.Bedrock.PrettyPrint (pretty)

import           Data.Bedrock
import           Data.Bedrock.Transform

-- stackFrameName :: Name
-- stackFrameName = Name ["bedrock"] "StackFrame" 0

cpsTransformation :: Gen ()
cpsTransformation = do
    -- pushNode $ NodeDefinition stackFrameName []
    fs <- gets (Map.elems . envFunctions)
    mapM_ cpsFunction fs

cpsFunction :: Function -> Gen ()
cpsFunction fn | NoCPS `elem` fnAttributes fn =
    pushFunction fn
cpsFunction fn = do
    let size = frameSize (fnBody fn)
    frameVar <- newVariable "bedrock.stackframe" FramePtr
    body <- cpsBlock size [] [] fn frameVar (fnBody fn)
    let bodyWithFrame =
            Bind [] (Alloc size) $
            -- Bind [frameVar] (Store (ConstructorName stackFrameName 0) []) $
            Bind [frameVar] (ReadRegister "hp") $
            Bind [] (BumpHeapPtr size) $
            Bind [] (Write frameVar 1 stdContinuation) $
            body
    let fn' = fn{fnArguments = fnArguments fn ++ [stdContinuation]
                ,fnResults = []
                ,fnBody = if size > 0 then bodyWithFrame else body}
    pushFunction fn'


cpsBlock :: NodeSize -> [Int] -> [Int] -> Function -> Variable -> Block -> Gen Block
cpsBlock size prims ptrs origin frameVar block =
  case block of
    Bind binds simple rest ->
      cpsExpression size prims ptrs origin frameVar binds simple rest
    Return args -> do
      node <- newVariable "returnAddr" IWord
      let fn_size = case fnResults origin of
                  [StaticNode n] -> n
                  _              -> length args
      -- We need to invoke with at least 1+'size' arguments.
      -- 'node' is repeated here. Could be Undefined as well. Doesn't matter.
      return $
        Bind [node] (Load stdContinuation 0) $
        Invoke node (stdContinuation : take fn_size (args ++ repeat node))
    Case scrut defaultBranch alternatives ->
      Case scrut
        <$> maybe (return Nothing) (fmap Just . cpsBlock size prims ptrs origin frameVar) defaultBranch
        <*> mapM (cpsAlternative size prims ptrs origin frameVar) alternatives
    -- Raise exception -> do
    --   node <- newVariable "contNode" Node
    --   return $
    --     Bind [node] (Fetch (stdContinuation size)) $
    --     InvokeHandler node exception
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
isCatchFrame _                 = False

cpsExpression :: NodeSize -> [Int] -> [Int] -> Function -> Variable -> [Variable]
             -> Expression -> Block -> Gen Block
cpsExpression size prims ptrs origin frameVar binds simple rest =
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
          <$> cpsBlock size prims ptrs origin frameVar rest
      MkNode (FunctionName fn blanks) args ->
        Bind binds (MkNode (FunctionName fn (blanks+1)) args)
          <$> cpsBlock size prims ptrs origin frameVar rest
      Save var n ->
        Bind binds (Write frameVar n var)
          <$> if isPrimitive var
              then cpsBlock size (n : prims) ptrs origin frameVar rest
              else cpsBlock size prims (n:ptrs) origin frameVar rest
      Restore n ->
        Bind binds (Load frameVar n) <$> cpsBlock size prims ptrs origin frameVar rest
      other -> Bind binds other <$> cpsBlock size prims ptrs origin frameVar rest
  where
    mkContinuation use = do

        fnPtr <- newVariable "fnPtr" (Primitive $ CPointer (CFunction CVoid [CPointer I8]))

        framePtr <- newVariable "bedrock.stackframe.cont" FramePtr
        prevFrame <- newVariable "bedrock.stackframe.cont" FramePtr
        prevFrameMarked <- newVariable "bedrock.stackframe.cont" FramePtr

        contFnName <- tagName "continuation" (fnName origin)

        -- gcFnName <- tagName "gc" contFnName

        -- pushHelper Function
        --   { fnName = gcFnName
        --   , fnAttributes = [NoCPS, Internal]
        --   , fnArguments = [framePtr]
        --   , fnResults = [FramePtr 0]
        --   , fnBody =
        --       Bind [prevFrame] (Load framePtr 1) $
        --       Bind [prevFrameMarked] (GCMarkFrame prevFrame) $
        --       Return [framePtr]
        --       -- Panic $ "GC not implemented for: " ++ show (pretty contFnName)
        --   }

        body <- cpsBlock size [] [] origin framePtr $
            Bind [stdContinuation] (Load framePtr 1) $ rest
        pushHelper $
            Function { fnName      = contFnName
                     -- , fnAttributes = [ AltReturn 1 gcFnName ]
                     , fnAttributes = [ Prefix size prims ptrs Nothing ]
                     , fnArguments = framePtr : binds
                     , fnResults   = []
                     , fnBody      = body }

        return $
            Bind [fnPtr] (FunctionPointer contFnName) $
            Bind [] (Write frameVar 0 fnPtr) $
            use frameVar

cpsAlternative :: NodeSize -> [Int] -> [Int] -> Function -> Variable -> Alternative -> Gen Alternative
cpsAlternative size prims ptrs origin frameVar (Alternative pattern expr) =
    case pattern of
        NodePat (FunctionName fn n) args ->
            Alternative
                (NodePat (FunctionName fn (n+1)) args)
                <$> cpsBlock size prims ptrs origin frameVar expr
        _ -> Alternative pattern <$> cpsBlock size prims ptrs origin frameVar expr



stdContinuation :: Variable
stdContinuation = Variable (Name [] "cont" 0) FramePtr

frameSize :: Block -> Int
frameSize block =
    case block of
        Bind _ Application{} rest -> max 2 (frameSize rest)
        Bind _ (Restore n) rest -> max (n+1) (frameSize rest)
        Bind _ _ rest -> frameSize rest
        Return{} -> 0
        Case _scrut defaultBranch alts -> maximum
            ((case defaultBranch of
                Nothing     -> 0
                Just branch -> frameSize branch) :
            [ frameSize branch | Alternative _ branch <- alts ])
        TailCall{} -> 0
        Exit -> 0
        _ -> error "Data.Bedrock.Exceptions.frameSize"

isPrimitive :: Variable -> Bool
isPrimitive var =
  case variableType var of
    NodePtr{}    -> False
    Node{}       -> False
    StaticNode{} -> False
    IWord{}      -> True
    Primitive{}  -> True
    FramePtr     -> False

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Bedrock.LLVM ( compile ) where

import           Control.Applicative    (Applicative)
import           Control.Monad.Reader
import           Data.Map               (Map)
import qualified Data.Map               as Map
import qualified LLVM.Wrapper.BitWriter as LLVM
import qualified LLVM.Wrapper.Core      as LLVM
import LLVM.Wrapper.Core ( Value )

import           Data.Bedrock

data Env = Env
    { envNodeMapping :: Map NodeName Int
    , envVariables   :: Map Variable LLVM.Value
    , envBuilder     :: LLVM.Builder
    , envFunction    :: Value
    , envModule      :: LLVM.Module
    , envWordTy      :: LLVM.Type
    , envVoidTy      :: LLVM.Type
    }

newtype Gen a = Gen { unGen :: ReaderT Env IO a }
    deriving
        ( Monad, Applicative, Functor, MonadReader Env
        , MonadIO )








compile :: Module -> IO ()
compile bedrock = do
    m <- mCreateWithName "lhc"
    cx <- LLVM.getGlobalContext
    voidTy <- LLVM.voidTypeInContext cx

    runGen bedrock m cx $ prepareFunctions (functions bedrock) $ \llvmFns -> do
        zipWithM_ compileFunction (functions bedrock) llvmFns



        
        let mainFnTy = LLVM.functionType voidTy [] False
        mainFn <- liftIO $ LLVM.addFunction m "main" mainFnTy
        entry <- liftIO $ LLVM.appendBasicBlock mainFn ""
        bld <- liftIO $ LLVM.createBuilder
        setFunction mainFn $ setBuilder bld $ do
            Just entryFn <- liftIO $ LLVM.getNamedFunction m (uniqueName $ entryPoint bedrock)
        
            liftIO $ LLVM.positionAtEnd bld entry
            zero <- constWord 0
            call <- buildCall entryFn [zero] ""
            liftIO $ LLVM.setInstructionCallConv call LLVM.Fast
            buildRetVoid
            return ()


    LLVM.writeBitcodeToFile m "output.bc"
    --LLVM.destroyModule m
    return ()

prepareFunctions :: [Function] -> ([Value] -> Gen a) -> Gen a
prepareFunctions fs0 action = worker [] fs0
  where
    worker acc []     = action (reverse acc)
    worker acc (f:fs) = prepareFunction f (\llvmFn -> worker (llvmFn:acc) fs)

prepareFunction :: Function -> (Value -> Gen a) -> Gen a
prepareFunction fn action = do
    wordTy <- asks envWordTy
    voidTy <- asks envVoidTy
    m <- asks envModule
    let fnReturnTy = voidTy
        fnArgTys = replicate (length (fnArguments fn)) wordTy
        fnTy = LLVM.functionType fnReturnTy fnArgTys False
    llvmFn <- liftIO $ LLVM.addFunction m (uniqueName (fnName fn)) fnTy
    liftIO $ LLVM.setLinkage llvmFn LLVM.PrivateLinkage
    liftIO $ LLVM.setFunctionCallConv llvmFn LLVM.Fast

    fnArgs <- forM (zip [0..] (fnArguments fn)) $ \(nth, arg) -> do
        let param = LLVM.getParam llvmFn nth
        liftIO $ LLVM.setValueName param (uniqueVariable arg)
        return (arg, param)

    bindVariables fnArgs (action llvmFn)

compileFunction :: Function -> Value -> Gen ()
compileFunction fn llvmFn = do
    entry <- liftIO $ LLVM.appendBasicBlock llvmFn "entry"
    bld <- liftIO $ LLVM.createBuilder
    liftIO $ LLVM.positionAtEnd bld entry
    setFunction llvmFn $ setBuilder bld $
        compileExpression (fnBody fn)

compileExpression :: Expression -> Gen ()
compileExpression expr =
    case expr of
        Bind [bind] (Unit [arg]) rest ->
            compileBind bind arg $
            compileExpression rest
        Bind [bind] (Load ptr nth) rest ->
            compileLoad bind ptr nth $
            compileExpression rest
        Bind [bind] (Store node args) rest ->
            compileStore bind node args $
            compileExpression rest
        
        -- Ignore GCAllocate and Alloc for now
        Bind [bind] GCAllocate{} rest -> do
            value <- constWord 1
            bindVariable bind value $ compileExpression rest
        Bind [] Alloc{} rest -> compileExpression rest
        Bind [] GCBegin{} rest -> compileExpression rest
        Bind [] GCEnd{} rest -> compileExpression rest
        Bind [bind] (GCMark ptr) rest -> do
            value <- resolve ptr
            bindVariable bind value $ compileExpression rest

        Bind [] (Print prim) rest -> do
            cx <- liftIO LLVM.getGlobalContext
            i8 <- liftIO $ LLVM.intTypeInContext cx 8
            i32 <- liftIO $ LLVM.intTypeInContext cx 32
            wordTy <- asks envWordTy
            let i8p = LLVM.pointerType i8 0
            let fnTy = LLVM.functionType i32 [i8p,wordTy] False
            m <- asks envModule
            printf <- liftIO $ LLVM.addFunction m "printf" fnTy
            string <- withBuilder $ \bld -> liftIO $ LLVM.buildGlobalStringPtr bld "string %d\n" ""
            arg <- resolve prim
            buildCall printf [string, arg] ""
            compileExpression rest
        Bind [bind] (Add a b) rest -> do
            value <- compileAdd a b
            bindVariable bind value $ compileExpression rest
        
        Bind binds (Unit args) rest ->
            compileExpression $
                foldr (\(b,a) -> Bind [b] (Unit [a])) rest (zip binds args)

        Case scrut _defaultBranch alts -> do
            self <- asks envFunction
            defBlock <- liftIO $ LLVM.appendBasicBlock self "default"
            defBuilder <- liftIO $ LLVM.createBuilder
            liftIO $ LLVM.positionAtEnd defBuilder defBlock
            liftIO $ LLVM.buildUnreachable defBuilder

            value <- resolve scrut
            switch <- buildSwitch value defBlock (length alts)
            forM_ alts $ \(Alternative pattern branch) -> do
                branchBlock <- liftIO $ LLVM.appendBasicBlock self "branch"
                branchBuilder <- liftIO $ LLVM.createBuilder
                liftIO $ LLVM.positionAtEnd branchBuilder branchBlock
                setBuilder branchBuilder $ compileExpression branch
                pValue <- compilePattern pattern
                liftIO $ LLVM.addCase switch pValue branchBlock
            return ()
        TailCall fn args -> do
            llvmFn <- resolveFunction fn
            llvmArgs <- mapM resolve args
            llvmCall <- buildCall llvmFn llvmArgs ""
            liftIO $ LLVM.setTailCall llvmCall True
            liftIO $ LLVM.setInstructionCallConv llvmCall LLVM.Fast
            buildRetVoid
            return ()
        Exit -> buildRetVoid >> return ()
        _ -> error $ "LLVM expr: " ++ show expr

compileAdd :: Variable -> Variable -> Gen Value
compileAdd a b = do
    a' <- resolve a
    b' <- resolve b
    buildAdd a' b' ""

compileStore :: Variable -> NodeName -> [Variable] -> Gen a -> Gen a
compileStore bind nodeName args action = do
    wordTy <- asks envWordTy
    size <- constWord (fromIntegral $ 1 + length args)
    ptrValue <- buildArrayMalloc wordTy size ""
    intValue <- buildPtrToInt ptrValue wordTy ""
    tagValue <- resolveNodeName nodeName
    buildStore tagValue ptrValue
    forM_ (zip [1..] args) $ \(nth, arg) -> do
        offset <- constWord nth
        offsetPtr <- buildGEP ptrValue [offset] ""
        argValue <- resolve arg
        buildStore argValue offsetPtr
    bindVariable bind intValue $ action

compileLoad :: Variable -> Variable -> Int -> Gen a -> Gen a
compileLoad bind ptr nth action = do
    wordTy <- asks envWordTy
    wordValue <- resolve ptr
    let pType = LLVM.pointerType wordTy 0
    
    ptrValue    <- buildIntToPtr wordValue pType ""
    offset      <- constWord (fromIntegral nth)
    offsetValue <- buildGEP ptrValue [offset] ""
    load        <- buildLoad offsetValue (uniqueVariable bind)
    bindVariable bind load action

compilePattern :: Pattern -> Gen LLVM.Value
compilePattern pattern = do
    case pattern of
        LitPat (LiteralInt i) -> constWord i
        NodePat name []       -> resolveNodeName name

compileBind :: Variable -> Argument -> Gen a -> Gen a
compileBind variable arg action = do
    value <- case arg of
        RefArg ref            -> resolve ref
        LitArg (LiteralInt i) -> constWord i
        NodeArg name _args    -> resolveNodeName name
    bindVariable variable value action



-------------------------------------------------
-- Helpers

runGen :: Module -> LLVM.Module -> LLVM.Context -> Gen a -> IO a
runGen m llvmModule cx gen = runReaderT (unGen gen) =<< mkEnv m llvmModule cx

bindVariable :: Variable -> LLVM.Value -> Gen a -> Gen a
bindVariable variable value = local $ \env ->
    env{ envVariables = Map.insert variable value (envVariables env) }

bindVariables :: [(Variable, LLVM.Value)] -> Gen a -> Gen a
bindVariables [] = id
bindVariables ((variable, value):rest) =
    bindVariable variable value . bindVariables rest

resolve :: Variable -> Gen LLVM.Value
resolve variable = do
    m <- asks envVariables
    case Map.lookup variable m of
        Nothing    -> error "LLVM.resolve"
        Just value -> return value

resolveNodeName :: NodeName -> Gen LLVM.Value
resolveNodeName nodeName = do
    wordTy <- asks envWordTy
    m <- asks envNodeMapping
    case Map.lookup nodeName m of
        Nothing  -> error "LLVM.resolveNodeName"
        Just idx -> return $ LLVM.constInt wordTy (fromIntegral idx) False

resolveFunction :: Name -> Gen Value
resolveFunction name = do
    m <- asks envModule
    Just fn <- liftIO $ LLVM.getNamedFunction m (uniqueName name)
    return fn

constWord :: Integer -> Gen LLVM.Value
constWord word = do
    wordTy <- asks envWordTy
    return $ LLVM.constInt wordTy (fromIntegral word) False

setBuilder :: LLVM.Builder -> Gen a -> Gen a
setBuilder bld = local $ \env -> env{ envBuilder = bld }

setFunction :: LLVM.Value -> Gen a -> Gen a
setFunction fn = local $ \env -> env{ envFunction = fn }

withBuilder :: (LLVM.Builder -> Gen a) -> Gen a
withBuilder action = action =<< asks envBuilder

mkEnv :: Module -> LLVM.Module -> LLVM.Context -> IO Env
mkEnv m llvmModule cx = do
    wordTy <- LLVM.intTypeInContext cx 64
    voidTy <- LLVM.voidTypeInContext cx
    return Env
        { envNodeMapping = Map.fromList $ flip zip [0..] $
            [ ConstructorName name | NodeDefinition name _tys <- nodes m ] ++
            [ FunctionName (fnName fn) blanks
            | fn <- functions m, blanks <- [0..length (fnArguments fn)] ]
        , envVariables = Map.empty
        , envFunction = error "envFunction not defined"
        , envBuilder = error "envBuilder not defined"
        , envModule = llvmModule
        , envWordTy = wordTy
        , envVoidTy = voidTy
        }

uniqueName :: Name -> String
uniqueName name =
    nameIdentifier name ++ "_" ++ show (nameUnique name)

uniqueVariable :: Variable -> String
uniqueVariable = uniqueName . variableName



-------------------------------------------------
-- LLVM Wrappers

buildRetVoid :: Gen Value
buildRetVoid = withBuilder $ \bld -> do
    liftIO $ LLVM.buildRetVoid bld

buildIntToPtr :: Value -> LLVM.Type -> String -> Gen Value
buildIntToPtr intValue ptrType name = withBuilder $ \bld ->
    liftIO $ LLVM.buildIntToPtr bld intValue ptrType name

buildPtrToInt :: Value -> LLVM.Type -> String -> Gen Value
buildPtrToInt ptrValue intType name = withBuilder $ \bld ->
    liftIO $ LLVM.buildPtrToInt bld ptrValue intType name

buildGEP :: Value -> [Value] -> String -> Gen Value
buildGEP ptrValue indices name = withBuilder $ \bld ->
    liftIO $ LLVM.buildGEP bld ptrValue indices name

buildLoad :: Value -> String -> Gen Value
buildLoad ptrValue name = withBuilder $ \bld ->
    liftIO $ LLVM.buildLoad bld ptrValue name

buildSwitch :: Value -> LLVM.BasicBlock -> Int -> Gen Value
buildSwitch scrut defaultBranch nBranches = withBuilder $ \bld ->
    liftIO $ LLVM.buildSwitch bld scrut defaultBranch (fromIntegral nBranches)

_buildArrayAlloca :: LLVM.Type -> Value -> String -> Gen Value
_buildArrayAlloca ty val name = withBuilder $ \bld ->
    liftIO $ LLVM.buildArrayAlloca bld ty val name

buildArrayMalloc :: LLVM.Type -> Value -> String -> Gen Value
buildArrayMalloc ty val name = withBuilder $ \bld ->
    liftIO $ LLVM.buildArrayMalloc bld ty val name

buildStore :: Value -> Value -> Gen Value
buildStore val ptr = withBuilder $ \bld ->
    liftIO $ LLVM.buildStore bld val ptr

buildCall :: Value -> [Value] -> String -> Gen Value
buildCall fn args name = withBuilder $ \bld ->
    liftIO $ LLVM.buildCall bld fn args name

buildAdd :: Value -> Value -> String -> Gen Value
buildAdd a b name = withBuilder $ \bld ->
    liftIO $ LLVM.buildAdd bld a b name

-- Sublime Text 2 does not like functions with 'module' in their name.
mCreateWithName :: String -> IO LLVM.Module
mCreateWithName = LLVM.moduleCreateWithName

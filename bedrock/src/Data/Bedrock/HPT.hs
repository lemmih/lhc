{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Haskell2010                #-}
{- Heap Points-To analysis -}
module Data.Bedrock.HPT
    ( runHPT
    , HPTResult(..)
    , NameSet
    , HeapLocationSet(..)
    , heapLocationSetUnions
    , Objects
    , setVariableSize
    , sizeOfVariable
    , sizeOfNode
    , sizeOfObjects
    , variableIndex
    , mergeObjectList
    , ppHPTResult
    ) where

import           Control.Applicative          (Applicative, (<$>))
import           Control.Monad.Reader         hiding (liftIO)
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Control.Monad.Writer         hiding (liftIO)
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IntSet
import           Data.List
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.STRef
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vector
import           Data.Vector.Mutable          (MVector)
import qualified Data.Vector.Mutable          as MVector
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import           Text.Printf


import           Data.Bedrock
import           Data.Bedrock.Exceptions      (isCatchFrame)
import           Data.Bedrock.Misc
import           Data.Bedrock.PrettyPrint
import           Data.Bedrock.Rename

import           Data.Bedrock.PrettyPrint     ()

data HPTResult = HPTResult
    { hptFnArgs              :: FnArgs
    , hptFnRets              :: FnRets
    , hptNodeArgs            :: NodeArgs
    , hptPtrScope            :: Vector HeapLocationSet
    , hptNodeScope           :: Vector Objects
    , hptHeap                :: Vector Objects
    , hptSharedVariables     :: Vector Bool
    , hptSharedHeapLocations :: Vector Bool
    } deriving (Eq)



data HPTState s = HPTState
    { envFnArgs              :: FnArgs
    , envFnRets              :: FnRets
    , envFnRaise             :: FnRaise
    , envNodeArgs            :: NodeArgs
    , envPtrScope            :: PtrScope s
    , envNodeScope           :: NodeScope s
    , envHeap                :: Heap s
    , envSharedVariables     :: PtrSharingMap s
    , envSharedHeapLocations :: HeapSharingMap s
    , envFreeHeapLocation    :: STRef s HeapLocation
    }
newtype HPT s a = HPT
    { unHPT :: WriterT (Endo (HPT s ())) (ReaderT (HPTState s) (ST s)) a }
    deriving
        ( Functor, Monad, Applicative
        , MonadWriter (Endo (HPT s ())), MonadReader (HPTState s) )

type HeapLocation = Int
-- Use newtypes?
newtype HeapLocationSet = HeapLocationSet { unHeapLocationSet :: IntSet }
    deriving (Eq, Show)
type NameSet = IntSet

-- Indexes for function argument an return variables.
-- created once, used once. Not used in iterations.
-- XXX: Not just used once. Should either only use once or use a
--      more efficient data structure.
type FnArgs = Map Name [Variable]
type FnRets = Map Name [Variable]
type FnRaise = Map Name Variable
type NodeArgs = Map Name [Variable]

-- Variables are shared if they're used more than once.
-- Heap locations are shared if referred to by a shared variable or
-- if they are updated into a shared heap location.
type PtrSharingMap s = MVector s Bool
type HeapSharingMap s = MVector s Bool

type Objects = Map NodeName (Vector NameSet)
type PtrScope s = MVector s HeapLocationSet
type NodeScope s = MVector s Objects
type Heap s = MVector s Objects

--type DirtyScope = MVector Bool
--type DirtyHeap  = MVector Bool



-- All identifiers are required to be unique.
runHPT :: Module -> HPTResult
runHPT mOrigin = runST (do
    let m = unique mOrigin
        nStores = countStores m
        fnArgs = Map.fromList
            [ (fnName fn, fnArguments fn) | fn <- functions m ]
        (fnRets, nextFree) = mkFnRets (modNamespace m) m
        (fnRaise, nextFree') = mkFnRaise nextFree m
        (nodeArgs, ns) = mkNodeArgs nextFree' m
        nPointers = nsNextPointerId ns
        nNodes = nsNextNodeId ns
    heapVector <- MVector.replicate nStores Map.empty
    heapLoc <- newSTRef 0

    ptrScope <- MVector.replicate nPointers (HeapLocationSet IntSet.empty)
    nodeScope <- MVector.replicate nNodes Map.empty
    sharedVariables <- MVector.replicate nPointers False
    sharedHeapLocations <- MVector.replicate nStores False
    let shared =
            [ var | (var, n) <- Map.toList (analyseUsage m)
            , n > 1, isPointerType (variableType var) ]
    forM_ shared $ \var ->
        MVector.write sharedVariables (variableIndex var) True
    let env = HPTState
            { envFnArgs = fnArgs
            , envFnRets = fnRets
            , envFnRaise = fnRaise
            , envNodeArgs = nodeArgs
            , envPtrScope = ptrScope
            , envNodeScope = nodeScope
            , envHeap = heapVector
            , envSharedVariables = sharedVariables
            , envSharedHeapLocations = sharedHeapLocations
            , envFreeHeapLocation = heapLoc }
        hpt = hptModule m
    iterEndo <- runReaderT (execWriterT (unHPT hpt)) env
    initHeap <- Vector.freeze (envHeap env)
    initPtrScope <- Vector.freeze (envPtrScope env)
    initNodeScope <- Vector.freeze (envNodeScope env)
    initSharedVars <- Vector.freeze (envSharedVariables env)
    initSharedHeap <- Vector.freeze (envSharedHeapLocations env)
    let iter = appEndo iterEndo (return ())
        hptResults = HPTResult
                { hptFnArgs = fnArgs
                , hptFnRets = fnRets
                , hptNodeArgs = nodeArgs
                , hptPtrScope = initPtrScope
                , hptNodeScope = initNodeScope
                , hptHeap = initHeap
                , hptSharedVariables = initSharedVars
                , hptSharedHeapLocations = initSharedHeap }
    runHPTLoop hptResults env iter)

runHPTLoop :: HPTResult -> HPTState s -> HPT s () -> ST s HPTResult
runHPTLoop oldResult env iter = do
    _ <- runReaderT (execWriterT (unHPT iter)) env
    newHeap <- Vector.freeze (envHeap env)
    newPtrScope <- Vector.freeze (envPtrScope env)
    newNodeScope <- Vector.freeze (envNodeScope env)
    newSharedVars <- Vector.freeze (envSharedVariables env)
    newSharedHeap <- Vector.freeze (envSharedHeapLocations env)
    let newResult = HPTResult
                { hptFnArgs = envFnArgs env
                , hptFnRets = envFnRets env
                , hptNodeArgs = envNodeArgs env
                , hptPtrScope = newPtrScope
                , hptNodeScope = newNodeScope
                , hptHeap = newHeap
                , hptSharedVariables = newSharedVars
                , hptSharedHeapLocations = newSharedHeap }
    -- XXX: Comparing the entire oldResult against the newResult is
    -- a bit silly.
    if oldResult == newResult
        then return newResult
        else runHPTLoop newResult env iter

ppHPTResult :: HPTResult -> IO ()
ppHPTResult hpt = do
    forM_ (zip [0..] (Vector.toList heap)) $ \(hp, hpValue) ->
        printf "HP[%d]: %s %s\n" (hp::Int) (ppObjects hpValue) (isSharedHP hp)
    forM_ (zip [0..] (Vector.toList ptrScope)) $ \(idx, HeapLocationSet ptrs) ->
        printf "*%d: %s %s\n" (idx::Int) (show $ IntSet.toList ptrs) (isSharedVar idx)
    forM_ (zip [0..] (Vector.toList nodeScope)) $ \(idx, objs) ->
        printf "%%%d: %s\n" (idx::Int) (ppObjects objs)
  where
    isSharedHP hp =
        if hptSharedHeapLocations hpt Vector.! hp
            then "(shared)"
            else ""
    isSharedVar var =
        if hptSharedVariables hpt Vector.! var
            then "(shared)"
            else ""
    heap = hptHeap hpt
    ptrScope = hptPtrScope hpt
    nodeScope = hptNodeScope hpt
    ppObjects :: Objects -> String
    ppObjects = intercalate " | " . map ppObject . Map.toList
    ppObject :: (NodeName, Vector NameSet) -> String
    ppObject (nodeName, args) =
        show (ppNode nodeName (map (Doc.text . show . IntSet.toList) (Vector.toList args)))


mkFnRets :: AvailableNamespace -> Module -> (FnRets, AvailableNamespace)
mkFnRets free m = worker free [] (functions m)
  where
    worker n acc [] = (Map.fromList acc, n)
    worker n acc (fn:fns) =
        let (rets, n') = assignID n [] (fnResults fn)
            acc' = (fnName fn, rets) : acc
        in worker n' acc' fns
    assignID n acc [] = (reverse acc, n)
    assignID n acc (ty:tys) =
        let (idNum, n') = newIDByType n ty
            var = Variable (Name [] "ret" idNum) ty
        in assignID n' (var:acc) tys

mkFnRaise :: AvailableNamespace -> Module -> (FnRaise, AvailableNamespace)
mkFnRaise free m = worker free [] (functions m)
  where
    worker n acc [] = (Map.fromList acc, n)
    worker n acc (fn:fns) =
        let (idNum, n') = newIDByType n Node
            elt = (fnName fn, Variable (Name [] "raise" idNum) Node)
        in worker n' (elt : acc) fns

mkNodeArgs :: AvailableNamespace -> Module -> (NodeArgs, AvailableNamespace)
mkNodeArgs free m = worker free [] (nodes m)
  where
    worker n acc [] = (Map.fromList acc, n)
    worker n acc (NodeDefinition name tys:ns) =
        let (rets, n') = assignID n [] tys
            acc' = (name, rets) : acc
        in worker n' acc' ns
    assignID n acc [] = (reverse acc, n)
    assignID n acc (ty:tys) =
        let (idNum, n') = newIDByType n ty
            var = Variable (Name [] "arg" idNum) ty
        in assignID n' (var:acc) tys

countStores :: Module -> Int
countStores = getSum . execWriter . mapM_ countFn . functions
  where
    countFn = countExpr . fnBody
    countExpr expr =
        case expr of
            Case _scrut _mbDefaultBranch alts ->
                mapM_ countAlt alts
            Bind _binds simple rest ->
                countSimple simple >> countExpr rest
            _ -> return ()
    countAlt (Alternative _pattern branch) = countExpr branch
    countSimple simple =
        case simple of
            Store{} -> tell (Sum 1)
            _       -> return ()



sizeOfVariable :: HPTResult -> Variable -> Int
sizeOfVariable hpt var =
    case variableType var of
        NodePtr      -> 1
        FramePtr     -> 1
        Node         -> sizeOfObjects hpt objects
        StaticNode n -> n
        Primitive{}  -> 1
  where
    objects = hptNodeScope hpt Vector.! variableIndex var

sizeOfObjects :: HPTResult -> Objects -> Int
sizeOfObjects hpt objects =
    foldr max 0 [ sizeOfNode hpt node | node <- Map.keys objects ]

sizeOfNode :: HPTResult -> NodeName -> Int
sizeOfNode hpt (ConstructorName node) =
    1 + sum (map (sizeOfVariable hpt) args)
  where
    args = hptNodeArgs hpt Map.! node
sizeOfNode hpt (FunctionName fn blanks) =
    1 + sum (map (sizeOfVariable hpt) args)
  where
    args = dropLast blanks (hptFnArgs hpt Map.! fn)

setVariableSize :: HPTResult -> Variable -> Variable
setVariableSize hpt var =
    case variableType var of
        Node -> var{variableType = StaticNode (sizeOfVariable hpt var)}
        _    -> var


getFunctionReturnRegisters :: Name -> HPT s [Variable]
getFunctionReturnRegisters name = do
    rets <- asks envFnRets
    case Map.lookup name rets of
        Nothing    -> error $ "Return registers not defined for: " ++
                              show name
        Just names -> return names

getFunctionArgumentRegisters :: Name -> HPT s [Variable]
getFunctionArgumentRegisters name = do
    args <- asks envFnArgs
    case Map.lookup name args of
        Nothing    -> error $ "Function arguments not defined for: " ++
                              show name
        Just names -> return names

getFunctionRaiseRegister :: Name -> HPT s Variable
getFunctionRaiseRegister name = do
    raises <- asks envFnRaise
    case Map.lookup name raises of
        Nothing -> error $ "HPT: Internal error"
        Just raise -> return raise

getNodeArgumentRegisters :: Name -> HPT s [Variable]
getNodeArgumentRegisters name = do
    args <- asks envNodeArgs
    case Map.lookup name args of
        Nothing -> error "Node argument not defined"
        Just names -> return names

isVariableShared :: Variable -> HPT s Bool
isVariableShared = isVariableShared' . variableIndex

isVariableShared' :: Int -> HPT s Bool
isVariableShared' index = do
    vector <- asks envSharedVariables
    liftST $ MVector.read vector index

markVariableShared :: Variable -> HPT s ()
markVariableShared = markVariableShared' . variableIndex

markVariableShared' :: Int -> HPT s ()
markVariableShared' index = do
    vector <- asks envSharedVariables
    liftST $ MVector.write vector index True

_isHeapLocationShared :: HeapLocation -> HPT s Bool
_isHeapLocationShared hpLocation = do
    vector <- asks envSharedHeapLocations
    liftST $ MVector.read vector hpLocation

markHeapLocationShared :: HeapLocation -> HPT s ()
markHeapLocationShared hpLocation = do
    vector <- asks envSharedHeapLocations
    liftST $ MVector.write vector hpLocation True

markHeapLocationSetShared :: HeapLocationSet -> HPT s ()
markHeapLocationSetShared (HeapLocationSet hpSet) =
    forM_ (IntSet.toList hpSet) $ markHeapLocationShared

singletonObject :: NodeName -> [Variable] -> Objects
singletonObject name vars =
    Map.singleton
        name
        (Vector.fromList $ map singletonNameSet vars )

singletonNameSet :: Variable -> NameSet
singletonNameSet var
    | isPrimitive (variableType var) = IntSet.empty
    | otherwise                      = IntSet.singleton (variableIndex var)
  where
    isPrimitive Primitive{} = True
    isPrimitive _ = False

variableIndex :: Variable -> Int
variableIndex = nameUnique . variableName

_liftIO :: IO a -> HPT s a
_liftIO = liftST . unsafeIOToST

liftST :: ST s a -> HPT s a
liftST action = HPT $ WriterT $ ReaderT $ \_ -> do
    ret <- action
    return (ret, mempty)

newHeapLocation :: HPT s HeapLocation
newHeapLocation = do
    hpRef <- asks envFreeHeapLocation
    hp <- liftST $ readSTRef hpRef
    liftST $ modifySTRef hpRef succ
    return hp

getNodeScope :: Variable -> HPT s Objects
getNodeScope = getNodeScope' . variableIndex

getNodeScope' :: Int -> HPT s Objects
getNodeScope' index = do
    scope <- asks envNodeScope
    liftST $ MVector.read scope index

getPtrScope :: Variable -> HPT s HeapLocationSet
getPtrScope = getPtrScope' . variableIndex

getPtrScope' :: Int -> HPT s HeapLocationSet
getPtrScope' index = do
    scope <- asks envPtrScope
    liftST $ MVector.read scope index

getHeapObjects :: HeapLocation -> HPT s Objects
getHeapObjects hp = do
    heap <- asks envHeap
    liftST $ MVector.read heap hp

setHeapObjects :: HeapLocation -> Objects -> HPT s ()
setHeapObjects hp objects = do
    heap <- asks envHeap
    liftST $ do
        oldObjects <- MVector.read heap hp
        MVector.write heap hp (mergeObjects objects oldObjects)

setHeapSetObjects :: HeapLocationSet -> Objects -> HPT s ()
setHeapSetObjects (HeapLocationSet hpSet) objects =
    forM_ (IntSet.toList hpSet) $ \hp -> setHeapObjects hp objects

setNodeScope :: Variable -> Objects -> HPT s ()
setNodeScope = setNodeScope' . variableIndex

setNodeScope' :: Int -> Objects -> HPT s ()
setNodeScope' index objects = do
    scope <- asks envNodeScope
    liftST $ do
        oldObjects <- MVector.read scope index
        MVector.write scope index (mergeObjects objects oldObjects)

setPtrScope :: Variable -> HeapLocationSet -> HPT s ()
setPtrScope var heapLocs = do
    scope <- asks envPtrScope
    liftST $ do
        oldPtrs <- MVector.read scope index
        MVector.write scope index (merge heapLocs oldPtrs)
  where
    merge (HeapLocationSet s1) (HeapLocationSet s2) =
        HeapLocationSet $ IntSet.union s1 s2
    index = variableIndex var

mergeObjects :: Objects -> Objects -> Objects
mergeObjects = Map.unionWith merge
  where
    merge = Vector.zipWith IntSet.union

mergeObjectList :: [Objects] -> Objects
mergeObjectList = Map.unionsWith merge
  where
    merge = Vector.zipWith IntSet.union

eachIteration :: HPT s () -> HPT s ()
eachIteration action = do
    -- Run the action once and the schedule it to be part of the
    -- fix-point iteration.
    action
    tell (Endo (action >>))

-- assert (variableType src == variableType dst)
hptCopyVariables :: Variable -> Variable -> HPT s ()
hptCopyVariables src dst | variableIndex src == variableIndex dst =
    return ()
hptCopyVariables src dst =
    case variableType src of
        Primitive{} -> return ()
        ty | isPointerType ty -> eachIteration $ do
            shared <- isVariableShared src
            when shared $ markVariableShared dst
            ptrs <- getPtrScope src
            setPtrScope dst ptrs
        ty | isNodeType ty -> eachIteration $ do
            objects <- getNodeScope src
            setNodeScope dst objects
        _ -> error "hptCopyVariables"

extract :: Objects -> NodeName -> Int -> NameSet
extract objs name nth =
    case Map.lookup name objs of
        Nothing    -> IntSet.empty
        Just names -> names Vector.! nth

hptAlternative :: Function -> Variable -> Alternative -> HPT s ()
hptAlternative origin scrut (Alternative pattern branch) = do
    case pattern of
        LitPat{} -> return ()
        NodePat name args ->
            forM_ (zip [0..] args) $ \(nth, arg) -> eachIteration $ do
                objects <- getNodeScope scrut
                let vals = IntSet.toList (extract objects name nth)
                case variableType arg of
                    Primitive{} -> return ()
                    NodePtr   ->
                        forM_ vals $ \ptr -> do
                            setOfPtrs <- getPtrScope' ptr
                            setPtrScope arg setOfPtrs
                    FramePtr   ->
                        forM_ vals $ \ptr -> do
                            setOfPtrs <- getPtrScope' ptr
                            setPtrScope arg setOfPtrs
                    Node      ->
                        forM_ vals $ \var -> do
                            setNodeScope arg =<< getNodeScope' var
                    StaticNode{} ->
                        error "HPT: Static node found during analysis."
    hptBlock origin branch

hptModule :: Module -> HPT s ()
hptModule = mapM_ hptFunction . functions

hptFunction :: Function -> HPT s ()
hptFunction fn = hptBlock fn (fnBody fn)

hptBlock :: Function -> Block -> HPT s ()
hptBlock origin block =
    case block of
        Case scrut defaultBranch alternatives -> do
            case defaultBranch of
                Nothing -> return ()
                Just branch -> hptBlock origin branch
            mapM_ (hptAlternative origin scrut) alternatives
        Bind binds simple rest -> do
            hptExpression origin binds simple
            hptBlock origin rest
        -- Copy the values from the vars out into the return registers for
        -- our function.
        Return vars -> do
            rets <- getFunctionReturnRegisters (fnName origin)
            forM_ (zip vars rets) $ uncurry hptCopyVariables
        Raise node -> do
            myRaise <- getFunctionRaiseRegister (fnName origin)
            hptCopyVariables node myRaise
        -- Copy values from 'fn' return registers into ours.
        -- copy values from vars into argument registers of 'fn'
        TailCall fn vars -> do
            originRets <- getFunctionReturnRegisters (fnName origin)
            foreignRets <- getFunctionReturnRegisters fn
            foreignArgs <- getFunctionArgumentRegisters fn
            forM_ (zip foreignRets originRets) $ uncurry hptCopyVariables
            forM_ (zip vars foreignArgs) $ uncurry hptCopyVariables
        Invoke obj args -> eachIteration $ do
            objects <- getNodeScope obj
            hptInvoke objects args
        InvokeHandler obj arg -> eachIteration $ do

            objects <- getNodeScope obj
            hptInvokeHandler objects arg
        Exit -> return ()
        Panic{} -> return ()

hptInvokeHandler :: Objects -> Variable -> HPT s ()
hptInvokeHandler objects arg = do
    frames <- stackTrace objects
    forM_ frames $ \frame ->
        case frame of
            FunctionStackFrame _fn _blanks (Just nextFramePtrs) -> do
                nextFrame <- getHeapSetObjects nextFramePtrs
                hptInvokeHandler nextFrame arg
            FunctionStackFrame _ _ Nothing -> error "HPT: End of stack"
            -- blanks must be 2
            CatchStackFrame exh blanks nextFramePtrs -> do
                exhArgs <- getFunctionArgumentRegisters exh
                let [exceptionArg, contArg] = takeLast blanks exhArgs
                eachIteration $
                    setPtrScope contArg nextFramePtrs
                hptCopyVariables arg exceptionArg

hptInvoke :: Objects -> [Variable] -> HPT s ()
hptInvoke objects args = do
    frames <- stackTrace objects
    forM_ frames $ \frame ->
        case frame of
            FunctionStackFrame fn blanks _nextFrame -> do
                fnArgs <- getFunctionArgumentRegisters fn
                let missingArgs = takeLast blanks fnArgs
                forM_ (zip args missingArgs) $
                    uncurry hptCopyVariables
            CatchStackFrame _exh _blanks nextFramePtrs -> do
                nextFrame <- getHeapSetObjects nextFramePtrs
                hptInvoke nextFrame args

derefPtrs :: NameSet -> HPT s HeapLocationSet
derefPtrs ptrs =
    heapLocationSetUnions <$> mapM getPtrScope' (IntSet.toList ptrs)

heapLocationSetUnions :: [HeapLocationSet] -> HeapLocationSet
heapLocationSetUnions = HeapLocationSet . IntSet.unions . map unHeapLocationSet

derefVars :: NameSet -> HPT s Objects
derefVars vars =
    mergeObjectList <$> mapM getNodeScope' (IntSet.toList vars)


getHeapSetObjects :: HeapLocationSet -> HPT s Objects
getHeapSetObjects (HeapLocationSet hps) = do
    mergeObjectList <$> mapM getHeapObjects (IntSet.toList hps)


data StackFrame
    = FunctionStackFrame Name Int (Maybe HeapLocationSet)
    | CatchStackFrame
        Name Int
        HeapLocationSet
    deriving (Show)
stackTrace :: Objects -> HPT s [StackFrame]
stackTrace objects = fmap concat $
    forM (Map.toList objects) $ \(name, objArgs) ->
        case name of
            FunctionName fn blanks -> do
                fnArgs <- getFunctionArgumentRegisters fn
                let isFrameVariable var = variableType var == FramePtr

                case findIndices isFrameVariable fnArgs of
                    -- Reach top-level and there are no more frames
                    [] -> return [FunctionStackFrame fn blanks Nothing]
                    [frameIdx] -> do
                        let stackPtrs = objArgs Vector.! frameIdx
                        stackHps <- derefPtrs stackPtrs
                        return [FunctionStackFrame fn blanks (Just stackHps)]
                    _ -> error "HPT: Too many FramePtr arguments"
            -- CatchFrame *normal_stack *handler
            ConstructorName cons | isCatchFrame cons -> do
                let stackPtrs = objArgs Vector.! 0
                    handlerVars = objArgs Vector.! 1
                stackHps <- derefPtrs stackPtrs

                handlerObjs <- derefVars handlerVars

                return
                    [ CatchStackFrame exh blanks stackHps
                    | FunctionName exh blanks <- Map.keys handlerObjs]
            _ -> error $ "stackTrace: " ++ show (name)

hptExpression :: Function -> [Variable] -> Expression -> HPT s ()
hptExpression origin binds simple =
    case simple of
        Store node vars | [ptr] <- binds -> do
            hp <- newHeapLocation
            setHeapObjects hp $ singletonObject node vars
            setPtrScope ptr (HeapLocationSet $ IntSet.singleton hp)
            case node of
                FunctionName fn _ -> do
                    args <- getFunctionArgumentRegisters fn
                    forM_ (zip vars args) $ uncurry hptCopyVariables
                ConstructorName name -> do
                    args <- getNodeArgumentRegisters name
                    forM_ (zip vars args) $ uncurry hptCopyVariables
        Fetch _constant ptrRef | [node] <- binds ->
            eachIteration $
                setNodeScope node =<< getHeapSetObjects =<< getPtrScope ptrRef
        TypeCast var | [bind] <- binds ->
            hptCopyVariables var bind
        MkNode node vars | [bind] <- binds -> do
            setNodeScope bind $ singletonObject node vars
            case node of
                FunctionName fn _ -> do
                    args <- getFunctionArgumentRegisters fn
                    forM_ (zip vars args) $ uncurry hptCopyVariables
                ConstructorName name -> do
                    args <- getNodeArgumentRegisters name
                    forM_ (zip vars args) $ uncurry hptCopyVariables
        Literal{} -> return ()
        Application fn vars -> do
            foreignRets <- getFunctionReturnRegisters fn
            foreignArgs <- getFunctionArgumentRegisters fn
            forM_ (zip vars foreignArgs) $ uncurry hptCopyVariables
            forM_ (zip foreignRets binds) $ uncurry hptCopyVariables
        CCall{} -> return ()
        Eval ptrRef | [objRef] <- binds -> eachIteration $ do
            shared <- isVariableShared ptrRef
            hpLocs <- getPtrScope ptrRef
            objects <- getHeapSetObjects hpLocs
            forM_ (Map.toList objects) $ \(nodeName, args) -> do
                case nodeName of
                    FunctionName name 0 -> do
                        [foreignRet] <- getFunctionReturnRegisters name
                        foreignObjs <- getNodeScope foreignRet
                        setNodeScope objRef foreignObjs
                        when shared $ do
                            setHeapSetObjects hpLocs foreignObjs
                            markHeapLocationSetShared hpLocs
                    _ -> setNodeScope objRef (Map.singleton nodeName args)
        Apply fn arg | [objRef] <- binds -> eachIteration $ do
            objects <- getNodeScope fn
            forM_ (Map.toList objects) $ \(nodeName, args) -> do
                case nodeName of
                    FunctionName name 1 -> do
                        [foreignRet] <- getFunctionReturnRegisters name
                        foreignObjs <- getNodeScope foreignRet
                        setNodeScope objRef foreignObjs
                        fnArgs <- getFunctionArgumentRegisters name
                        hptCopyVariables arg (last fnArgs)
                    FunctionName name n | n > 1 -> do
                        let newObjects = Map.singleton (FunctionName name (n-1)) (Vector.snoc args (IntSet.singleton (variableIndex arg)))
                        setNodeScope objRef newObjects
                        fnArgs <- getFunctionArgumentRegisters name
                        hptCopyVariables arg (fnArgs!!(length fnArgs-n))
                    _ -> error $ "invalid apply: " ++ show (objRef, nodeName)
        Add{} -> return ()
        GCAllocate{} -> return ()
        GCBegin -> return ()
        GCEnd -> return ()
        GCMark var | [ptr] <- binds ->
            hptCopyVariables var ptr
        GCMarkNode node | [bind] <- binds ->
            hptCopyVariables node bind
        Catch exh exhVars fn fnVars -> do
            exhRets <- getFunctionReturnRegisters exh
            fnRets  <- getFunctionReturnRegisters fn

            fnArgs <- getFunctionArgumentRegisters fn
            forM_ (zip fnVars fnArgs) $ uncurry hptCopyVariables

            exhArgs <- getFunctionArgumentRegisters exh
            forM_ (zip exhVars exhArgs) $ uncurry hptCopyVariables

            fnRaise <- getFunctionRaiseRegister fn
            exhRaise <- getFunctionRaiseRegister exh
            myRaise <- getFunctionRaiseRegister (fnName origin)
            -- exh catches all exceptions from 'fn'
            -- we throw all exceptions raised by 'exh'
            hptCopyVariables fnRaise (last exhArgs)
            hptCopyVariables exhRaise myRaise
            -- FIXME: copy (exceptions of fn) (last exhArgs)

            forM_ (zip exhRets binds) $ uncurry hptCopyVariables
            forM_ (zip fnRets binds) $ uncurry hptCopyVariables
        Alloc{} -> return ()
        Write{} -> error $ "HPT: Deal with Write"
        Address{} -> error $ "HPT: Deal with Address"
        Load{} -> error $ "HPT: Deal with Load"
        ReadRegister{} -> return ()
        WriteRegister{} -> return ()
        ReadGlobal{} -> return ()
        WriteGlobal{} -> return ()
        _ -> error $ "HPT: unHPandled simple: " ++ show simple



newtype SumMap = SumMap { unSumMap :: Map Variable Int }
instance Monoid SumMap where
    mempty = SumMap mempty
    mappend a b = SumMap $ Map.unionWith (+) (unSumMap a) (unSumMap b)
analyseUsage :: Module -> Map Variable Int
analyseUsage m =
    unSumMap (execWriter (analyseModule m))
  where
    push var = tell $ SumMap $ Map.singleton var 1
    pushMany vars = tell $ SumMap $ Map.fromList (zip vars [1,1..])
    analyseModule = mapM_ analyseFunction . functions
    analyseFunction = analyseBlock . fnBody
    analyseBlock block =
        case block of
            Case scrut _defaultBranch alts -> do
                push scrut
                mapM_ analyseAlternative alts
            Bind _binds expr rest -> do
                analyseExpression expr
                analyseBlock rest
            Return vars         -> pushMany vars
            Raise var           -> push var
            TailCall _fn vars   -> pushMany vars
            Invoke v1 vs        -> pushMany (v1:vs)
            InvokeHandler v1 v2 -> pushMany [v1,v2]
            Exit                -> return ()
            Panic{}             -> return ()
    analyseAlternative (Alternative _ block) = analyseBlock block
    analyseExpression expr =
        case expr of
            Application _fn vars   -> pushMany vars
            CCall _fn vars         -> pushMany vars
            Catch _ exhVars _ vars -> pushMany exhVars >> pushMany vars
            Alloc{}                -> return ()
            Store _name vars       -> pushMany vars
            Write v1 _ v2          -> pushMany [v1,v2]
            Address var _          -> push var
            Fetch _constant var    -> push var
            Load _constant var _   -> push var
            Add v1 v2              -> pushMany [v1,v2]
            ReadRegister{}         -> return ()
            WriteRegister _ var    -> push var
            ReadGlobal{}           -> return ()
            WriteGlobal _ var      -> push var
            TypeCast var           -> push var
            MkNode _name vars      -> pushMany vars
            Literal{}              -> return ()
            Eval var               -> push var
            Apply v1 v2            -> pushMany [v1,v2]
            GCAllocate{}           -> return ()
            GCBegin{}              -> return ()
            GCEnd{}                -> return ()
            GCMark var             -> push var
            GCMarkNode var         -> push var
    









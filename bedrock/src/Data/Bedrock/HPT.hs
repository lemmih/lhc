{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Haskell2010                #-}
{- Heap Points-To analysis -}
module Data.Bedrock.HPT
    ( runHPT
    , HPTResult(..)
    , NameSet
    , HeapPtrSet
    , Objects
    , setVariableSize
    , sizeOfNode
    , sizeOfObjects
    , variableIndex
    , mergeObjectList
    , ppHPTResult
    ) where

import           Control.Applicative          ((<$>), Applicative)
import           Control.Monad.Reader hiding ( liftIO )
import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Control.Monad.Writer hiding ( liftIO )
import           Data.IntSet                  (IntSet)
import qualified Data.IntSet                  as IntSet
import           Data.List
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.STRef
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as Vector
import           Data.Vector.Mutable          (IOVector, MVector)
import qualified Data.Vector.Mutable          as MVector
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import           Text.Printf


import           Data.Bedrock
import           Data.Bedrock.Misc
import           Data.Bedrock.PrettyPrint
import           Data.Bedrock.Rename
import           Data.Bedrock.Exceptions (isCatchFrame)


data HPTResult = HPTResult
    { hptFnArgs    :: FnArgs
    , hptFnRets    :: FnRets
    , hptNodeArgs  :: NodeArgs
    , hptPtrScope  :: Vector HeapPtrSet
    , hptNodeScope :: Vector Objects
    , hptHeap      :: Vector Objects
    }



data HPTState s = HPTState
    { envFnArgs              :: FnArgs
    , envFnRets              :: FnRets
    , envFnRaise             :: FnRaise
    , envNodeArgs            :: NodeArgs
    , envPtrScope            :: PtrScope s
    , envNodeScope           :: NodeScope s
    , envHeap                :: Heap s
    , envSharedVariables     :: PtrSharingMap
    , envSharedHeapLocations :: HeapSharingMap
    , envFreeHeapPtr         :: STRef s HeapPtr
    }
newtype HPT s a = HPT
    { unHPT :: WriterT (Endo (HPT s ())) (ReaderT (HPTState s) (ST s)) a }
    deriving
        ( Functor, Monad, Applicative
        , MonadWriter (Endo (HPT s ())), MonadReader (HPTState s) )

type HeapPtr = Int
-- Use newtypes?
type HeapPtrSet = IntSet
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
type PtrSharingMap = IOVector Bool
-- Set Name
type HeapSharingMap = IOVector Bool
-- Set HeapPtr

type Objects = Map NodeName (Vector NameSet)
type PtrScope s = MVector s HeapPtrSet
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
    heapPtr <- newSTRef 0

    ptrScope <- MVector.replicate nPointers IntSet.empty
    nodeScope <- MVector.replicate nNodes Map.empty
    let env = HPTState
            { envFnArgs = fnArgs
            , envFnRets = fnRets
            , envFnRaise = fnRaise
            , envNodeArgs = nodeArgs
            , envPtrScope = ptrScope
            , envNodeScope = nodeScope
            , envHeap = heapVector
            , envSharedVariables = error "shared vars"
            , envSharedHeapLocations = error "shared heaps"
            , envFreeHeapPtr = heapPtr }
        hpt = hptModule m
    iterEndo <- runReaderT (execWriterT (unHPT hpt)) env
    initHeap <- Vector.freeze (envHeap env)
    initPtrScope <- Vector.freeze (envPtrScope env)
    initNodeScope <- Vector.freeze (envNodeScope env)
    let iter = appEndo iterEndo (return ())
    runHPTLoop initHeap initPtrScope initNodeScope env iter)

runHPTLoop :: Vector Objects -> Vector HeapPtrSet -> Vector Objects
           -> HPTState s -> HPT s () -> ST s HPTResult
runHPTLoop oldHeap oldPtrScope oldNodeScope env iter = do
    -- putStrLn "Iteration..."
    _ <- runReaderT (execWriterT (unHPT iter)) env
    newHeap <- Vector.freeze (envHeap env)
    newPtrScope <- Vector.freeze (envPtrScope env)
    newNodeScope <- Vector.freeze (envNodeScope env)
    if oldHeap == newHeap && oldPtrScope == newPtrScope &&
       oldNodeScope == newNodeScope
        then do
            -- ppEnv newHeap newScope
            return HPTResult
                { hptFnArgs = envFnArgs env
                , hptFnRets = envFnRets env
                , hptNodeArgs = envNodeArgs env
                , hptPtrScope = newPtrScope
                , hptNodeScope = newNodeScope
                , hptHeap = newHeap }
        else runHPTLoop newHeap newPtrScope newNodeScope env iter

ppHPTResult :: HPTResult -> IO ()
ppHPTResult hpt = do
    forM_ (zip [0..] (Vector.toList heap)) $ \(hp, hpValue) ->
        printf "HP[%d]: %s\n" (hp::Int) (ppObjects hpValue)
    forM_ (zip [0..] (Vector.toList ptrScope)) $ \(idx, ptrs) ->
        printf "*%d: %s\n" (idx::Int) (show $ IntSet.toList ptrs)
    forM_ (zip [0..] (Vector.toList nodeScope)) $ \(idx, objs) ->
        printf "%%%d: %s\n" (idx::Int) (ppObjects objs)
  where
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



sizeOfNode :: HPTResult -> Variable -> Int
sizeOfNode hpt var = sizeOfObjects objects
  where
    objects = hptNodeScope hpt Vector.! variableIndex var

sizeOfObjects :: Objects -> Int
sizeOfObjects objects =
    foldr max 0 (map Vector.length (Map.elems objects)) + 1

setVariableSize :: HPTResult -> Variable -> Variable
setVariableSize hpt var =
    case variableType var of
        Node -> var{variableType = StaticNode (sizeOfNode hpt var)}
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

newHeapPtr :: HPT s HeapPtr
newHeapPtr = do
    hpRef <- asks envFreeHeapPtr
    hp <- liftST $ readSTRef hpRef
    liftST $ modifySTRef hpRef succ
    return hp

getNodeScope :: Variable -> HPT s Objects
getNodeScope = getNodeScope' . variableIndex

getNodeScope' :: Int -> HPT s Objects
getNodeScope' index = do
    scope <- asks envNodeScope
    liftST $ MVector.read scope index

getPtrScope :: Variable -> HPT s HeapPtrSet
getPtrScope = getPtrScope' . variableIndex

getPtrScope' :: Int -> HPT s HeapPtrSet
getPtrScope' index = do
    scope <- asks envPtrScope
    liftST $ MVector.read scope index

getHeapObjects :: HeapPtr -> HPT s Objects
getHeapObjects hp = do
    heap <- asks envHeap
    liftST $ MVector.read heap hp

setHeapObjects :: HeapPtr -> Objects -> HPT s ()
setHeapObjects hp objects = do
    heap <- asks envHeap
    liftST $ do
        oldObjects <- MVector.read heap hp
        MVector.write heap hp (mergeObjects objects oldObjects)

setNodeScope :: Variable -> Objects -> HPT s ()
setNodeScope = setNodeScope' . variableIndex

setNodeScope' :: Int -> Objects -> HPT s ()
setNodeScope' index objects = do
    scope <- asks envNodeScope
    liftST $ do
        oldObjects <- MVector.read scope index
        MVector.write scope index (mergeObjects objects oldObjects)

setPtrScope :: Variable -> HeapPtrSet -> HPT s ()
setPtrScope var heapPtrs = do
    scope <- asks envPtrScope
    liftST $ do
        oldPtrs <- MVector.read scope index
        MVector.write scope index (IntSet.union heapPtrs oldPtrs)
  where
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
        NodePtr -> do
            eachIteration $ do
                ptrs <- getPtrScope src
                setPtrScope dst ptrs
        FramePtr -> do
            eachIteration $ do
                ptrs <- getPtrScope src
                setPtrScope dst ptrs
        Node -> do
            eachIteration $ do
                objects <- getNodeScope src
                setNodeScope dst objects
        StaticNode{} -> return ()

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
            FunctionStackFrame _fn _blanks nextFramePtrs -> do
                nextFrame <- derefHeap nextFramePtrs
                hptInvokeHandler nextFrame arg
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
                nextFrame <- derefHeap nextFramePtrs
                hptInvoke nextFrame args

derefPtrs :: NameSet -> HPT s HeapPtrSet
derefPtrs ptrs =
    IntSet.unions <$> mapM getPtrScope' (IntSet.toList ptrs)

derefHeap :: HeapPtrSet -> HPT s Objects
derefHeap hps = do
    mergeObjectList <$> mapM getHeapObjects (IntSet.toList hps)


data StackFrame
    = FunctionStackFrame Name Int HeapPtrSet
    | CatchStackFrame
        Name Int
        HeapPtrSet
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
                    [] -> return [FunctionStackFrame fn blanks undefined]
                    [frameIdx] -> do
                        let stackPtrs = objArgs Vector.! frameIdx
                        stackHps <- derefPtrs stackPtrs
                        return [FunctionStackFrame fn blanks stackHps]
                    _ -> error "HPT: Too many FramePtr arguments"
            -- CatchFrame *normal_stack *handler
            ConstructorName cons | isCatchFrame cons -> do
                let stackPtrs = objArgs Vector.! 0
                    handlerPtrs = objArgs Vector.! 1
                stackHps <- derefPtrs stackPtrs
                
                handlerHps <- derefPtrs handlerPtrs
                handlerObjs <- derefHeap handlerHps

                return
                    [ CatchStackFrame exh blanks stackHps
                    | FunctionName exh blanks <- Map.keys handlerObjs]
            _ -> error $ "stackTrace: " ++ show (name)

hptExpression :: Function -> [Variable] -> Expression -> HPT s ()
hptExpression origin binds simple =
    case simple of
        Store node vars | [ptr] <- binds -> do
            hp <- newHeapPtr
            setHeapObjects hp $ singletonObject node vars
            setPtrScope ptr (IntSet.singleton hp)
            case node of
                FunctionName fn _ -> do
                    args <- getFunctionArgumentRegisters fn
                    forM_ (zip vars args) $ uncurry hptCopyVariables
                ConstructorName name -> do
                    args <- getNodeArgumentRegisters name
                    forM_ (zip vars args) $ uncurry hptCopyVariables
        Fetch ptrRef | [node] <- binds -> do
            eachIteration $ do
                ptrs <- IntSet.toList <$> getPtrScope ptrRef
                objects <- mapM getHeapObjects ptrs
                setNodeScope node (mergeObjectList objects)
        Unit arg | [bind] <- binds -> do
            case arg of
                -- All such simple renamings should have been removed
                -- for performance reasons.
                RefArg var -> hptCopyVariables var bind
                LitArg{} -> return ()
                NodeArg name vars ->
                    setNodeScope bind $ singletonObject name vars
        Application fn vars -> do
            foreignRets <- getFunctionReturnRegisters fn
            foreignArgs <- getFunctionArgumentRegisters fn
            forM_ (zip vars foreignArgs) $ uncurry hptCopyVariables
            forM_ (zip foreignRets binds) $ uncurry hptCopyVariables
        CCall{} -> return ()
        Eval ptrRef | [objRef] <- binds -> eachIteration $ do
            ptrs <- IntSet.toList <$> getPtrScope ptrRef
            objects <- mergeObjectList <$> mapM getHeapObjects ptrs
            forM_ (Map.toList objects) $ \(nodeName, args) -> do
                case nodeName of
                    FunctionName name 0 -> do
                        [foreignRet] <- getFunctionReturnRegisters name
                        foreignObjs <- getNodeScope foreignRet
                        setNodeScope objRef foreignObjs
                        -- FIXME: We only need to update the ptrs if
                        --   there is sharing.
                        forM_ ptrs $ \ptr -> setHeapObjects ptr foreignObjs
                    _ -> setNodeScope objRef (Map.singleton nodeName args)
        Apply fn arg | [objRef] <- binds -> eachIteration $ do
            -- fn is node
            -- arg is nodePtr
            -- objRef is node
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

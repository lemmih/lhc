{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE PatternGuards              #-}
module Data.Bedrock.HPT where

import           Control.Applicative
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap
import           Data.IntSet         (IntSet)
import qualified Data.IntSet         as IntSet
import           Data.Map            (Map)
import qualified Data.Map     as Map
--import           Data.Set            (Set)
--import qualified Data.Set     as Set
import           Control.Monad.RWS.Strict
import           Data.IORef
import           Data.Vector         (Vector)
import qualified Data.Vector         as Vector

import           Data.Bedrock


import           GHC.Exts

class QuickEq a where
    quickEq :: a -> a -> Bool

instance (QuickEq a, QuickEq b) => QuickEq (a,b) where
    quickEq (la,lb) (ra,rb) = quickEq la ra && quickEq lb rb

instance QuickEq a => QuickEq [a] where
    quickEq [] [] = True
    quickEq (l:ls) (r:rs) = quickEq l r && quickEq ls rs
    quickEq _ _ = False

instance QuickEq (IntMap a) where
    quickEq a b =
        case reallyUnsafePtrEquality# a b of
            0# -> False
            _# -> True

instance QuickEq (Map a b) where
    quickEq a b =
        case reallyUnsafePtrEquality# a b of
            0# -> False
            _# -> True

instance QuickEq IntSet where
    quickEq a b =
        case reallyUnsafePtrEquality# a b of
            0# -> False
            _# -> True

{- Steps:
1. construct initial sharing map for variables.
2. Pass bedrock code through the run-time compiler.
3. Iterate the resulting function until no more progress can be made.
-}

{- Pathological case

long_list = [1,2,3,4,5,6,7,8,9,10,undefined]
val = sum long_list
sum [] = 0
sum (x:xs) = x + sum xs
-}

data World = World
    { worldFnArgs              :: !FnArgs
    , worldFnRets              :: !FnRets
    , worldSharedVariables     :: !PtrSharingMap
    , worldSharedHeapLocations :: !HeapSharingMap
    , worldNodeScope           :: !NodeScope
    , worldPtrScope            :: !PtrScope
    , worldHeap                :: !Heap
    , worldHeapIndex           :: !HeapPtr -- Next available heap pointer
    }
newtype HPT a = HPT { unHPT :: RWST () (Endo (HPT ())) World IO a }
    deriving
        ( Functor, Monad, MonadIO, MonadState World
        , MonadWriter (Endo (HPT ())) )

type HeapPtr = Int
-- Use newtypes?
type HeapPtrSet = IntSet
type NameSet = IntSet

-- Indexes for function argument an return variables.
-- created once, used once. Not used in iterations.
type FnArgs = IntMap [Variable]
type FnRets = IntMap [Variable]

-- Variables are shared if they're used more than once.
-- Heap locations are shared if referred to by a shared variable or
-- if they are updated into a shared heap location.
type PtrSharingMap = NameSet
-- Set Name
type HeapSharingMap = HeapPtrSet
-- Set HeapPtr

-- XXX: Use HashMap?
type Objects = Map NodeName (Vector NameSet)
type PtrScope = IntMap HeapPtrSet
-- Map Name (Set HeapPtr)
type NodeScope = IntMap Objects
-- Map Name Objects
type Heap = IntMap Objects
-- Map HeapPtr Objects

getFunctionReturnRegisters :: Name -> HPT [Variable]
getFunctionReturnRegisters name = do
    rets <- gets worldFnRets
    case IntMap.lookup (nameUnique name) rets of
        Nothing    -> error $ "Return registers not defined for: " ++
                              show name
        Just names -> return names

getFunctionArgumentRegisters :: Name -> HPT [Variable]
getFunctionArgumentRegisters name = do
    args <- gets worldFnArgs
    case IntMap.lookup (nameUnique name) args of
        Nothing    -> error $ "Function arguments not defined for: " ++
                              show name
        Just names -> return names

singletonObject :: NodeName -> [Variable] -> Objects
singletonObject name vars =
    Map.singleton
        name
        (Vector.fromList $ map singletonNameSet vars )

singletonNameSet :: Variable -> NameSet
singletonNameSet var
    | variableType var == Primitive = IntSet.empty
    | otherwise                     = IntSet.singleton (variableIndex var)

variableIndex :: Variable -> Int
variableIndex = nameUnique . variableName

newHeapPtr :: HPT HeapPtr
newHeapPtr = do
    hp <- gets worldHeapIndex
    modify $ \st -> st{ worldHeapIndex = hp+1 }
    return hp

getNodeScope :: Variable -> HPT Objects
getNodeScope var = do
    scope <- gets worldNodeScope
    case IntMap.lookup (variableIndex var) scope of
        Nothing      -> error $ "Missing variable: " ++ show var
        Just objects -> return objects

getPtrScope :: Variable -> HPT HeapPtrSet
getPtrScope var = do
    scope <- gets worldPtrScope
    case IntMap.lookup (variableIndex var) scope of
        Nothing   -> error $ "Missing variable: " ++ show var
        Just ptrs -> return ptrs

getHeapObjects :: HeapPtr -> HPT Objects
getHeapObjects hp = do
    heap <- gets worldHeap
    case IntMap.lookup hp heap of
        Nothing      -> error $ "Invalid heap pointer: " ++ show hp
        Just objects -> return objects

setHeapObjects :: HeapPtr -> Objects -> HPT ()
setHeapObjects hp objects = modify $ \st -> st{ worldHeap =
    IntMap.insertWith mergeObjects hp objects (worldHeap st) }

setNodeScope :: Variable -> Objects -> HPT ()
setNodeScope var objects = modify $ \st -> st{ worldNodeScope =
    IntMap.insertWith mergeObjects index objects (worldNodeScope st) }
  where
    index = variableIndex var

setPtrScope :: Variable -> HeapPtrSet -> HPT ()
setPtrScope var heapPtrs = modify $ \st -> st{ worldPtrScope =
    IntMap.insertWith IntSet.union index heapPtrs (worldPtrScope st) }
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

eachIteration :: HPT () -> HPT ()
eachIteration action = do
    -- Run the action once and the schedule it to be part of the
    -- fix-point iteration.
    action
    tell (Endo (action >>))

mkDependency :: (QuickEq a, Monoid a) => HPT (a -> HPT () -> HPT ())
mkDependency = do
    ref <- liftIO $ newIORef mempty
    return $ \newValue action -> do
        oldValue <- liftIO $ readIORef ref
        if oldValue `quickEq` newValue
            then liftIO (writeIORef ref newValue) >> action
            else return ()

-- assert (variableType src == variableType dst)
hptCopyVariables :: Variable -> Variable -> HPT ()
hptCopyVariables src dst | variableIndex src == variableIndex dst =
    return ()
hptCopyVariables src dst =
    case variableType src of
        Primitive -> return ()
        NodePtr -> do
            dep <- mkDependency
            eachIteration $ do
                ptrs <- getPtrScope src
                dep ptrs $
                    setPtrScope dst ptrs 
        Node -> do
            dep <- mkDependency
            eachIteration $ do
                objects <- getNodeScope src
                dep objects $
                    setNodeScope dst objects

hptExpression :: Function -> Expression -> HPT ()
hptExpression origin expr =
    case expr of
        Case scrut _defaultBranch alternatives -> undefined scrut alternatives
        Bind binds simple rest -> do
            hptSimpleExpression binds simple
            hptExpression origin rest
        -- Copy the values from the vars out into the return registers for
        -- our function.
        Return vars -> do
            rets <- getFunctionReturnRegisters (fnName origin)
            forM_ (zip vars rets) $ uncurry hptCopyVariables
        Throw{} ->
            error "Calls to @throw must have been lower before doing HPT\
                  \ analysis."
        -- Copy values from 'fn' return registers into ours.
        -- copy values from vars into argument registers of 'fn'
        TailCall fn vars -> do
            originRets <- getFunctionReturnRegisters (fnName origin)
            foreignRets <- getFunctionReturnRegisters fn
            foreignArgs <- getFunctionArgumentRegisters fn
            forM_ (zip foreignRets originRets) $ uncurry hptCopyVariables
            forM_ (zip vars foreignArgs) $ uncurry hptCopyVariables
        Exit -> return ()
        Panic{} -> return ()

hptSimpleExpression :: [Variable] -> SimpleExpression -> HPT ()
hptSimpleExpression binds simple =
    case simple of
        Store node vars | [ptr] <- binds -> do
            hp <- newHeapPtr
            setHeapObjects hp $ singletonObject node vars
            setPtrScope ptr (IntSet.singleton hp)
        Fetch ptrRef | [node] <- binds -> do
            dep <- mkDependency
            eachIteration $ do
                ptrs <- IntSet.toList <$> getPtrScope ptrRef
                objects <- mapM getHeapObjects ptrs
                -- If the objects haven't changed, do not update the scope.
                dep objects $
                    setNodeScope node (mergeObjectList objects)
        Unit args -> forM_ (zip binds args) $ \(bind, arg) -> do
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
{-
*hp := @Store (Nothing)
ptr <- newHeapPtr
setHeap ptr (Node Nothing [])
setPtrScope *hp (IntSet.singleton ptr)

%var := @Unit( %other )
rhs <- queryScope (Variable %other)
setScope (Variable %var) rhs

@update oldObject newObject
-- 'oldObject' is a variable which may point to 'ptrs'
ptrs <- queryPtrScope oldObject
newPtrs <- queryPtrScope newObject
dependencies (ptrs, newPtrs) $
  forM_ ptrs $ \ptr ->
    setHeap ptr (nodes from newPtrs)

%node := Fetch *ptr
objects <- getHeap *ptr
dependency objects $ -- Don't run setScope if objects haven't changed.
    setScope (Variable %node) (Object objects)  

case %scrut of
    Pointer *ptr -> ...
    Prim #prim -> ...
    Node %node -> ...
objects <- queryNodeScope %scrut
let firsts = extract first args objects

-- method for ptrs
for firsts $ \ptr ->
    setOfPtrs <- queryPtrScope ptr
    setPtrScope %obj setOfPtrs

-- prim is ignored.

-- method for nodes
for firsts $ \objectRef ->
    objects <- queryNodeScope objectRef
    setScope %node objects

-}


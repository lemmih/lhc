{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}
-- FIXME: Use HashSet instead of IntSet.
module Grin.Stage2.DeadCode
    ( trimDeadCode
    , calcLiveNodes
    ) where

import Grin.Stage2.Types

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Debug.Trace

calcLiveNodes :: Grin -> IO ()
calcLiveNodes grin
    = do let live = liveNodes grin
         writeFile "livenodes.txt" (unlines (map show (IntSet.toList live)))

trimDeadCode :: Grin -> Grin
trimDeadCode grin
    = grin { grinFunctions = map walkFunc [ fn | fn <- grinFunctions grin, nodeId (funcDefName fn) `IntSet.member` liveSet]
           , grinCAFs      = [ caf | caf <- grinCAFs grin, nodeId (cafName caf) `IntSet.member` liveSet ]
           , grinNodes     = [ node | node <- grinNodes grin, nodeId (nodeName node) `IntSet.member` liveSet ]
           }
    where walkFunc func
              = func { funcDefBody = walkExp (funcDefBody func) }
          walkExp (e1@Case{} :>>= binds :-> e2)
              = walkExp e1 :>>= binds :-> walkExp e2
          walkExp (e1@(Application (Builtin "update") args) :>>= binds :-> e2) | all isAlive args || True
              = walkExp e1 :>>= binds :-> walkExp e2
          walkExp (e1 :>>= binds :-> e2)
              = if all isDead binds
                then walkExp e2
                else walkExp e1 :>>= binds :-> walkExp e2
          walkExp (Case scrut alts)
              = if nodeId scrut `IntSet.member` liveSet || True
                then Case scrut (map walkAlt alts)
                else Unit []
          walkExp fn@(Application (Builtin "update") (ptr:_))
              | nodeId ptr `IntSet.member` liveSet || True
              = fn
              | otherwise
              = Unit []
          walkExp fn = fn
          walkAlt (alt :> exp) = alt :> walkExp exp
          liveSet = liveNodes grin
          isDead x = nodeId x `IntSet.notMember` liveSet
          isAlive = not . isDead

liveNodes :: Grin -> IntSet.IntSet
liveNodes grin
    = let entryPoint = nodeId (grinEntryPoint grin)
          graph = execSM (grinGraph grin) entryPoint IntMap.empty
      in reachable entryPoint graph

reachable :: Int -> DependencyGraph -> IntSet.IntSet
reachable entry graph
    = loop (IntSet.singleton entry) (IntSet.singleton entry)
    where loop marked new | IntSet.null new = marked
          loop marked new
              = let reachableByNew = IntSet.unions [ find node | node <- IntSet.toList new ]
                    unmarkedNew = reachableByNew `IntSet.difference` marked
                in loop (marked `IntSet.union` unmarkedNew) unmarkedNew
          find key = IntMap.findWithDefault IntSet.empty key graph



newtype SM a = SM { runSM :: Int -> DependencyGraph -> (a, DependencyGraph) }

instance Monad SM where
    return x = SM $ \r s -> (x, s)
    f >>= g  = SM $ \r s -> case runSM f r s of
                              (a, !s') -> runSM (g a) r s'

instance MonadState (IntMap.IntMap IntSet.IntSet) SM where
    get = SM $ \_ s -> (s, s)
    put s = SM $ \_ _ -> ((), s)

instance MonadReader Int SM where
    ask = SM $ \r s -> (r, s)
    local fn m = SM $ \r s -> runSM m (fn r) s

execSM action r s
    = case runSM action r s of
        (a, s) -> s

type DependencyGraph = IntMap.IntMap IntSet.IntSet

type M a = SM a

top :: M Int
top = ask

grinGraph :: Grin -> M ()
grinGraph grin
    = do mapM_ cafGraph (grinCAFs grin)
         mapM_ funcGraph (grinFunctions grin)

insert k v m = let v' = IntMap.findWithDefault IntSet.empty k m
               in IntMap.insertWith IntSet.union k v m

cafGraph :: CAF -> M ()
cafGraph caf
    = do let deps = valueGraph (cafValue caf)
         modify $ insert (nodeId (cafName caf)) deps
         return ()

funcGraph :: FuncDef -> M ()
funcGraph func
    = do bodyDeps <- local (const (nodeId (funcDefName func))) $ expGraph (funcDefBody func)
         modify $ insert (nodeId (funcDefName func)) bodyDeps
         return ()

expGraph :: Expression -> M IntSet.IntSet
expGraph (Unit vals)
    = return $ IntSet.fromList (map nodeId vals)
expGraph (e1 :>>= binds :-> e2)
    = do deps <- expGraph e1
         forM_ binds $ \bind -> modify $ insert (nodeId bind) deps
         expGraph e2
expGraph (Application (Builtin "updateMutVar") [ptr, val, realWorld])
    = do --modify $ insert (nodeId ptr) (IntSet.singleton (nodeId val))
         --modify $ insert (nodeId realWorld) (IntSet.singleton (nodeId ptr))
         return $ IntSet.fromList [nodeId realWorld, nodeId ptr, nodeId val]
expGraph (Application (Builtin "update") args)
    = do t <- top
         let s = IntSet.fromList (map nodeId args)
         modify $ insert t s
         return IntSet.empty
expGraph (Application fn args)
    = return $ IntSet.fromList (map nodeId (fn:args))
expGraph (Case scrut alts)
    = do t <- top
         modify $ insert t (IntSet.singleton (nodeId scrut))
         depss <- mapM altGraph alts
         forM_ depss $ \deps ->
          do modify $ insert (nodeId scrut) deps
             forM_ (IntSet.toList deps) $ \dep ->
               modify $ insert dep (IntSet.singleton (nodeId scrut))
         return $ IntSet.singleton (nodeId scrut)
expGraph (Fetch _idx hp)
    = return $ IntSet.singleton (nodeId hp)
expGraph (Store _size vals)
    = return $ IntSet.fromList (map nodeId vals)
expGraph (StoreHole _size)
    = return IntSet.empty
expGraph (Constant value)
    = return $ valueGraph value

nodeId :: Renamed -> Int
nodeId (Aliased uid _name) = uid
nodeId (Anonymous uid) = uid
nodeId (Builtin{}) = -1
nodeId (External{}) = -1

altGraph :: Alt -> M IntSet.IntSet
altGraph (value :> exp)
    = IntSet.union (valueGraph value) `liftM` expGraph exp

valueGraph :: Value -> IntSet.IntSet
valueGraph (Node tag ConstructorNode _partial) = IntSet.singleton (nodeId tag)
valueGraph (Node tag FunctionNode _partial) = IntSet.singleton (nodeId tag)
valueGraph Lit{} = IntSet.empty
valueGraph Hole = IntSet.empty
valueGraph Empty = IntSet.empty


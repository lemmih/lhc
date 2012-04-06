{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}
-- FIXME: Use HashSet instead of IntSet.
module Grin.PreciseDeadCode
    ( trimDeadCode
    ) where

import Grin.Types

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Debug.Trace

trimDeadCode :: Grin -> Grin
trimDeadCode grin
    = grin { grinFunctions = map walkFunc [ fn | fn <- grinFunctions grin, isAlive (funcDefName fn) ]
           , grinCAFs      = [ caf | caf <- grinCAFs grin, isAlive (cafName caf) ]
           , grinNodes     = [ node | node <- grinNodes grin, isAlive (nodeName node) ]
           }
    where walkFunc func
              = func { funcDefBody = walkExp (funcDefBody func) }
          walkExp (e1 :>> e2)
              = walkExp e1 :>> walkExp e2
          walkExp (e1 :>>= bind :-> e2)
              = if isDead bind
                then walkExp e2
                else walkExp e1 :>>= bind :-> walkExp e2
          walkExp fn@(Update size ptr val)
              | nodeId ptr `IntSet.member` liveSet
              = fn
              | otherwise
              = Unit Empty
          walkExp fn@(Application (Builtin "caseUpdate") [ptr, val])
              | nodeId ptr `IntSet.member` liveSet
              = fn
              | otherwise
              = Unit Empty
          walkExp (Case scrut alts)
              = Case scrut (map walkAlt alts)
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
    = do deps <- valueGraph (cafValue caf)
         modify $ insert (nodeId (cafName caf)) deps
         return ()

funcGraph :: FuncDef -> M ()
funcGraph func
    = do bodyDeps <- local (const (nodeId (funcDefName func))) $ expGraph (funcDefBody func)
         modify $ insert (nodeId (funcDefName func)) bodyDeps
         return ()

expGraph :: Expression -> M IntSet.IntSet
expGraph (Unit val)
    = valueGraph val
expGraph (e1 :>>= bind :-> e2)
    = do deps <- expGraph e1
         modify $ insert (nodeId bind) deps
         expGraph e2
expGraph (e1 :>> e2)
    = do expGraph e1
         expGraph e2
expGraph (Application (Builtin "updateMutVar") [ptr, val, realWorld])
    = do return $ IntSet.fromList [nodeId realWorld, nodeId ptr, nodeId val]
expGraph (Update size ptr val)
    = do t <- top
         let s = IntSet.singleton (nodeId val)
         modify $ insert (nodeId ptr) s
         return IntSet.empty
expGraph (Application (Builtin "caseUpdate") [ptr, val])
    = do t <- top
         let s = IntSet.singleton (nodeId val)
         modify $ insert (nodeId ptr) s
         return IntSet.empty
expGraph (Application fn args)
    = return $ IntSet.fromList (map nodeId (fn:args))
expGraph (Case scrut alts)
    = do t <- top
         modify $ insert t (IntSet.singleton (nodeId scrut))
         depss <- mapM altGraph alts
         forM_ depss $ \deps ->
           modify $ insert (nodeId scrut) deps
         return $ IntSet.singleton (nodeId scrut)
expGraph (Store size val)
    = valueGraph val

nodeId :: Renamed -> Int
nodeId (Aliased uid _name) = uid
nodeId (Anonymous uid) = uid
nodeId (Builtin{}) = -1
nodeId (External{}) = -1

altGraph :: Alt -> M IntSet.IntSet
altGraph (value :> exp)
    = liftM2 (IntSet.union) (valueGraph value) (expGraph exp)

valueGraph :: Value -> M IntSet.IntSet
valueGraph (Node tag _nt _partial args) = return $ IntSet.fromList (nodeId tag : map nodeId args)
valueGraph (Vector vs) = return $ IntSet.fromList (map nodeId vs)
valueGraph Lit{} = return IntSet.empty
valueGraph Hole{} = return IntSet.empty
valueGraph Empty = return IntSet.empty
valueGraph (Variable v) = return $ IntSet.singleton (nodeId v)


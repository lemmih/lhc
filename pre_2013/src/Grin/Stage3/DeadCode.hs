{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}
-- FIXME: Use HashSet instead of IntSet.
module Grin.Stage3.DeadCode
    ( trimDeadCode
    ) where

import Grin.Stage3.Types

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Debug.Trace

trimDeadCode :: Grin -> Grin
trimDeadCode grin
    = grin { grinFunctions = map walkFunc [ fn | fn <- grinFunctions grin, nodeId (funcDefName fn) `IntSet.member` liveSet]
           , grinCAFs      = [ caf | caf <- grinCAFs grin, nodeId (cafName caf) `IntSet.member` liveSet ]
           , grinNodes     = [ node | node <- grinNodes grin, nodeId (nodeName node) `IntSet.member` liveSet ]
           }
    where walkFunc func
              = func { funcDefBody = walkExp (funcDefBody func)
                     , funcDefArgs = filter isAlive (funcDefArgs func) }
          walkExp (Case scrut alts)
              = Case scrut [ cond :> worker simple
                           | cond :> simple <- alts ]
          walkExp (Singleton simple)
              = Singleton $ worker simple
          walkExp (update@(Application (Builtin "update") _args) :>>= binds :-> rest)
              = update :>>= binds :-> walkExp rest
          walkExp (simple :>>= binds :-> rest)
              = if all isDead binds
                then walkExp rest
                else worker simple :>>= binds :-> walkExp rest

          argLiveMap = Map.fromList [ ((funcDefName func, n), isAlive arg)
                                    | func <- grinFunctions grin
                                    , (n, arg) <- zip [0..] (funcDefArgs func) ]
          worker (Application fn args) = Application fn (filterLiveArgs fn args)
          worker simple = simple

          filterLiveArgs fn args = [ arg | (n, arg) <- zip [0..] args, Map.findWithDefault True (fn,n) argLiveMap ]

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
expGraph (e1 :>>= binds :-> e2)
    = do deps <- simpleGraph e1
         forM_ binds $ \bind -> modify $ insert (nodeId bind) deps
         expGraph e2
expGraph (Case scrut alts)
    = do t <- top
         modify $ insert t (IntSet.singleton (nodeId scrut))
         deps <- mapM (\(cond :> simple) -> do dep <- simpleGraph simple
                                               return $ valueGraph cond `IntSet.union` dep) alts
         modify $ insert (nodeId scrut) $ IntSet.unions deps
         return $ IntSet.singleton (nodeId scrut)
expGraph (Singleton simple)
    = simpleGraph simple

simpleGraph :: SimpleExpression -> M IntSet.IntSet
simpleGraph (Unit vals)
    = return $ IntSet.fromList (map nodeId vals)
simpleGraph (Application (Builtin "updateMutVar") [ptr, val, realWorld])
    = do --modify $ insert (nodeId ptr) (IntSet.singleton (nodeId val))
         --modify $ insert (nodeId realWorld) (IntSet.singleton (nodeId ptr))
         return $ IntSet.fromList [nodeId realWorld, nodeId ptr, nodeId val]
simpleGraph (Application (Builtin "update") args)
    = do t <- top
         let s = IntSet.fromList (map nodeId args)
         modify $ insert t s
         return IntSet.empty
simpleGraph (Application fn args)
    = return $ IntSet.fromList (map nodeId (fn:args))
simpleGraph (Fetch _idx hp)
    = return $ IntSet.singleton (nodeId hp)
simpleGraph (Store _size vals)
    = return $ IntSet.fromList (map nodeId vals)
simpleGraph (StoreHole _size)
    = return IntSet.empty
simpleGraph (Constant value)
    = return $ valueGraph value



nodeId :: Renamed -> Int
nodeId (Aliased uid _name) = uid
nodeId (Anonymous uid) = uid
nodeId (Builtin{}) = -1
nodeId (External{}) = -1

valueGraph :: Value -> IntSet.IntSet
valueGraph (Node tag ConstructorNode _partial) = IntSet.singleton (nodeId tag)
valueGraph (Node tag FunctionNode _partial) = IntSet.singleton (nodeId tag)
valueGraph Lit{} = IntSet.empty
valueGraph Hole = IntSet.empty
valueGraph Empty = IntSet.empty


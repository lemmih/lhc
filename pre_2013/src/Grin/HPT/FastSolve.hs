{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Grin.HPT.FastSolve
    ( solve
    ) where

import Grin.Types                         ( Renamed(..), NodeType(..) )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State.Strict

import Grin.HPT.Environment as Env
import Grin.HPT.Interface as Interface
import qualified Grin.HPT.Interface as Interface

import Grin.Stage2.Pretty (ppRenamed)

import Debug.Trace

import Data.HashMap (HashMap)
import Data.HashSet (HashSet)

import qualified Data.HashMap as HM
import qualified Data.HashSet as HS


data HPTState
    = HPTState { hptAnalysis :: HeapAnalysis
               , hptLiveSet :: HashMap Lhs Env.Rhs
               , hptChanged :: !Bool
               }
type M = State HPTState

type Minner a = ReaderT Lhs M a

type SharingMap = Map.Map Lhs Bool


solve :: Equations -> ([HeapAnalysis], HeapAnalysis)
solve eqs
    = let iterate i
              = do live <- return (reverse $ Map.toList eqs) -- gets (HM.toList . hptLiveSet)
                   forM_ live $ \(lhs,rhs) ->
                     do debugMsg $ "Reducing: " ++ ppLhs lhs ++ " " ++ show rhs
                        reducedRhs <- runReaderT (reduceEqs rhs) lhs
                        addReduced lhs reducedRhs
                        --d <- isDead rhs
                        --when d $ modify $ \st -> st{hptLiveSet = HM.delete lhs (hptLiveSet st)}
                        return ()
          bootSequence
              = do live <- gets hptLiveSet
                   forM_ (HM.toList live) $ \(lhs,rhs) ->
                     do reducedRhs <- runReaderT (reduceEqs rhs) lhs
                        modify $ \st -> st{ hptAnalysis = hptAddBinding lhs reducedRhs (hptAnalysis st) }
          loop iter prev
              = case execState (iterate iter) prev of
                  (newData) ->
                    if not (hptChanged newData) -- hptAnalysis prev == hptAnalysis newData
                    then ([hptAnalysis newData], hptAnalysis newData) else
                    let (iterList, finishedData) = loop (iter+1) newData{hptChanged = False}
                    in  (hptAnalysis newData : iterList, finishedData)
          initState = HPTState { hptAnalysis = mkHeapAnalysis (Map.map (const mempty) eqs) (nonlinearVariables eqs)
                               , hptLiveSet = HM.fromList (Map.toList eqs)
                               , hptChanged = False }
          firstState = execState bootSequence initState
      in loop 1 firstState{ hptChanged = False }

isDead :: Env.Rhs -> M Bool
isDead (Rhs rhs) = do ds <- mapM worker rhs
                      return (and ds)
    where worker Env.Base = return True
          worker (Ident i) = do live <- gets hptLiveSet
                                return (not $ HM.member (VarEntry i) live)
          --worker (Env.Heap{}) = return True
          worker _    = return False

-- Scan for shared variables. A variable is shared if it is used more than once.
-- Detecting shared heap points is done later when we solve the equations.
nonlinearVariables :: Equations -> SharingMap
nonlinearVariables eqs
    = appEndo (execWriter (mapM_ rhsFn (Map.elems eqs))) Map.empty
    where rhsFn (Rhs values) = mapM_ worker values
          pushIdent ident = tell $ Endo $ Map.insertWith (\_ _ -> True) (VarEntry ident) False
          worker (Extract ident (tag, _nt, _missing) _nth)   = pushIdent ident >> pushIdent tag
          worker (ExtractVector ident _nth) = pushIdent ident
          worker (Eval ident)               = pushIdent ident
          worker (Update a b)               = pushIdent a >> pushIdent b
          worker (Apply a b)                = pushIdent a >> pushIdent b
          worker (PartialApply a b)         = return ()
          worker (Ident ident)              = pushIdent ident
          worker (Fetch ident)              = pushIdent ident
          worker Env.Base                   = return ()
          worker Env.Heap{}                 = return ()
          worker (Tag tag _nt _nargs args)  = pushIdent tag >> mapM_ rhsFn args
          worker (VectorTag args)           = mapM_ rhsFn args

debugMsg :: Monad m => String -> m ()
debugMsg str
    = return () -- trace str (return ())

ppLhs :: Lhs -> String
ppLhs (VarEntry v)   = show (ppRenamed v)
ppLhs (HeapEntry hp) = "@" ++ show hp


addReduced :: (MonadState HPTState m) => Lhs -> Interface.Rhs -> m ()
addReduced lhs rhs
    = do orig <- lookupEqAtomic lhs
         let noNewChanges = rhs `Interface.isSubsetOf` orig
         unless noNewChanges $
           do modify $ \st -> st{ hptAnalysis = hptAddBinding lhs rhs (hptAnalysis st) }
              modify $ \st -> st{ hptChanged = True }
              debugMsg $ ppLhs lhs ++ ":"
              debugMsg $ "Old: " ++ show orig
              debugMsg $ "Rhs: " ++ show rhs
              debugMsg $ "New: " ++ show (mappend orig rhs)
              --setDirty lhs
              shared <- isShared lhs
              when shared $
                mapM_ setShared (listHeapPointers rhs)


listHeapPointers :: Interface.Rhs -> [HeapPointer]
listHeapPointers (Interface.Heap hps) = Set.toList hps
listHeapPointers _ = []


reduceEqs :: Env.Rhs -> Minner Interface.Rhs
reduceEqs (Rhs rhs) = do rhs' <- mapM reduceEq rhs
                         return $ mconcat rhs'

reduceEq :: RhsValue -> Minner Interface.Rhs
reduceEq Env.Base  = return $ Interface.Base
reduceEq (Env.Heap hp) = return $ Interface.Heap (Set.singleton hp)
reduceEq (Ident i) = lookupDirtyEq (VarEntry i)
reduceEq (Extract eq node n) = reduceExtract eq node n
reduceEq (ExtractVector eq n)
    = do rhs <- lookupEq (VarEntry eq)
         case rhs of
           Interface.Empty -> return mempty
           Interface.Other {rhsVector = args} ->
             return (args `nth` n)
    where nth [] n = error $ "reduceEq: ExtractVector: " ++ show (eq, n)
          nth (x:xs) 0 = x
          nth (x:xs) n = nth xs (n-1)
reduceEq (Tag t nt missing args)
    = do args' <- mapM reduceEqs args
         return $ Other (Map.singleton (t, nt, missing) args') []
reduceEq (VectorTag args)
    = do args' <- mapM reduceEqs args
         return $ Interface.Other Map.empty args'
reduceEq (Eval i) = reduceEval i
reduceEq (Fetch i)
    = do rhs <- lookupEq (VarEntry i)
         case rhs of
           Interface.Heap hp -> cautionDirty (VarEntry i) $ liftM mconcat (mapM (lookupEq . HeapEntry) (Set.toList hp))
           Empty -> return Empty
reduceEq (Apply a b) = reduceApply a b
reduceEq (PartialApply a b)
    = do rhs <- lookupEq (VarEntry a)
         case rhs of
           Empty -> return Empty
           Other{rhsTagged = nodes} ->
             do let f ((tag, nt, n), args)
                      | n == 0    = return mempty
                      | otherwise = do bRhs <- lookupDirtyEq (VarEntry b)
                                       return $ Other (Map.singleton (tag, nt, (n-1)) (args ++ [bRhs])) []
                cautionDirty (VarEntry a) $ liftM mconcat $ mapM f (Map.toList nodes)
reduceEq (Update hp val)
    = do rhs <- lookupEq (VarEntry hp)
         case rhs of
           Interface.Empty -> return mempty
           Interface.Heap hps ->
             do valRhs  <- cautionDirty (VarEntry hp) $ lookupDirtyEq (VarEntry val)
                forM_ (Set.toList hps) $ \hp -> addReduced (HeapEntry hp) valRhs
                return mempty

reduceExtract eq node n
    = do rhs <- lookupEq (VarEntry eq)
         case rhs of
           Interface.Empty -> return mempty
           Other{rhsTagged = nodes} ->
             return (Map.findWithDefault [] node nodes `nth` n)
    where nth [] n = mempty
          nth (x:xs) 0 = x
          nth (x:xs) n = nth xs (n-1)

reduceEval i
    = do hpt <- gets hptAnalysis
         rhs <- lookupEq (VarEntry i)
         case rhs of
           Interface.Base -> return Interface.Base
           Interface.Empty -> return Interface.Empty
           Interface.Heap hps ->
             do let anyShared = heapIsShared i hpt
                let fn hp = do let worker ((t, FunctionNode, 0), args) = do rhs <- lookupDirtyEq (VarEntry t)
                                                                            when (anyShared && rhs /= mempty) $
                                                                              addReduced (HeapEntry hp) rhs
                                                                            return rhs
                                   worker ((t, nt, missing), args)     = return $ Other (Map.singleton (t, nt, missing) args) []
                               hpRhs <- lookupEq (HeapEntry hp)
                               case hpRhs of
                                 Empty        -> return mempty
                                 Other{rhsTagged = nodes} -> liftM mconcat $ mapM worker (Map.toList nodes)
                cautionDirty (VarEntry i) $ liftM mconcat $ mapM fn (Set.toList hps)
           rhs -> error $ "Eval: " ++ show (rhs, i)

reduceApply a b
    = do rhs <- lookupEq (VarEntry a)
         case rhs of
          Empty -> return Empty
          Other{rhsTagged = nodes} ->
            do let f ((func, FunctionNode, 1), args)
                     = lookupDirtyEq (VarEntry func)
                   f ((conc, nt, n), args)
                       | n == 0    = return mempty
                       | otherwise = do bRhs <- lookupDirtyEq (VarEntry b)
                                        return $ Other (Map.singleton (conc, nt, (n-1)) (args ++ [bRhs])) []
               cautionDirty (VarEntry a) $ liftM mconcat $ mapM f (Map.toList nodes)


lookupDirtyEq :: Lhs -> Minner Interface.Rhs
lookupDirtyEq lhs = lookupEq lhs
{-
lookupDirtyEq lhs
    = do isClean <- gets (HS.member lhs . hptClean)
         allDirty <- gets ((/=) 0 . hptAllDirty)
         if isClean && not allDirty
            then return Empty
            else lookupEq lhs
-}

lookupEq :: Lhs -> Minner Interface.Rhs
lookupEq lhs
    = do --addDependency lhs
         gets $ \st -> lookupLhs lhs (hptAnalysis st)

cautionDirty :: Lhs -> Minner a -> Minner a
cautionDirty _ action = action
{-
cautionDirty lhs action
    = do isClean <- gets (HS.member lhs . hptClean)
         if isClean then action else
           do modify $ \st -> st{hptAllDirty = succ (hptAllDirty st) }
              r <- action
              modify $ \st -> st{hptAllDirty = pred (hptAllDirty st) }
              return r
-}

lookupEqAtomic :: MonadState HPTState m => Lhs -> m Interface.Rhs
lookupEqAtomic lhs
    = gets $ \st -> lookupLhs lhs (hptAnalysis st)

isShared :: MonadState HPTState m => Lhs -> m Bool
isShared lhs
    = gets $ \st -> hptIsShared lhs (hptAnalysis st)

setShared :: MonadState HPTState m => HeapPointer ->m ()
setShared hp = modify $ \st ->st{hptAnalysis = hptSetShared (HeapEntry hp) (hptAnalysis st)}



{-# LANGUAGE OverloadedStrings #-}
module Grin.HPT.QuickSolve
    ( solve
    ) where

import Grin.Types                         ( Renamed(..), NodeType(..) )

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State.Strict

import Grin.HPT.Environment as Env
import Grin.HPT.Interface as Interface
import qualified Grin.HPT.Interface as Interface

import Grin.Stage2.Pretty (ppRenamed)

--import Tick
import Debug.Trace

import Control.Parallel.Strategies

type M a = State HeapAnalysis a

type SharingMap = Map.Map Lhs Bool


solve :: Equations -> [HeapAnalysis]
solve eqs
    = let eqPairs = Map.toList eqs
          iterate i ls
              = forM_ ls $ \(lhs,rhs) ->
                  do debugMsg $ "Reducing: " ++ ppLhs lhs ++ " " ++ show i
                     reducedRhs <- reduceEqs rhs
                     addReduced lhs reducedRhs
          loop iter prev
              = case execState (debugMsg ("Iteration: " ++ show iter) >> iterate iter eqPairs) prev of
                  (newData) -> -- | rnf newData `seq` True ->
                    if prev == newData
                      then [newData]
                      else {- prev -} undefined : loop (iter+1) newData
      in loop 1 (mkHeapAnalysis (Map.map (const mempty) eqs) (nonlinearVariables eqs))

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

debugMsg :: String -> M ()
debugMsg str
    = return () -- trace str (return ())

ppLhs :: Lhs -> String
ppLhs (VarEntry v)   = show (ppRenamed v)
ppLhs (HeapEntry hp) = "@" ++ show hp


addReduced :: Lhs -> Interface.Rhs -> M ()
addReduced lhs rhs
    = do orig <- {-addTick "AddReduced" $ -} lookupEq lhs
         let noNewChanges = rhs `Interface.isSubsetOf` orig
         unless noNewChanges $
           do {-addTick "HPT: Change" $ -}
              modify $ \hpt -> hptAddBinding lhs rhs hpt
              debugMsg $ ppLhs lhs ++ ":"
              --debugMsg $ "Old: " ++ show orig
              --debugMsg $ "Rhs: " ++ show rhs
              debugMsg $ "New: " ++ show (mappend orig rhs)
              shared <- isShared lhs
              when shared $
                mapM_ setShared (listHeapPointers rhs)

listHeapPointers :: Interface.Rhs -> [HeapPointer]
listHeapPointers (Interface.Heap hps) = Set.toList hps
listHeapPointers _ = []


reduceEqs :: Env.Rhs -> M Interface.Rhs
reduceEqs (Rhs rhs) = do rhs' <- mapM reduceEq rhs
                         return $ mconcat rhs'

reduceEq :: RhsValue -> M Interface.Rhs
reduceEq Env.Base  = return $ Interface.Base
reduceEq (Env.Heap hp) = return $ Interface.Heap (Set.singleton hp)
reduceEq (Ident i) = lookupEq (VarEntry i)
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
    = do hpt <- get
         return $ lookupHeap i hpt
reduceEq (Apply a b) = reduceApply a b
reduceEq (PartialApply a b)
    = do rhs <- lookupEq (VarEntry a)
         case rhs of
           Empty -> return Empty
           Other{rhsTagged = nodes} ->
             do let f ((tag, nt, n), args)
                      | n == 0    = return mempty
                      | otherwise = do bRhs <- lookupEq (VarEntry b)
                                       return $ Other (Map.singleton (tag, nt, (n-1)) (args ++ [bRhs])) []
                    f t             = error $ "reduceEq: apply: " ++ show t
                liftM mconcat $ mapM f (Map.toList nodes)
reduceEq (Update hp val)
    = do rhs <- lookupEq (VarEntry hp)
         case rhs of
           Interface.Empty -> return mempty
           Interface.Heap hps ->
             do valRhs  <- lookupEq (VarEntry val)
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
    = do hpt <- get
         case lookupLhs (VarEntry i) hpt of
           Interface.Base -> return Interface.Base
           Interface.Empty -> return Interface.Empty
           Interface.Heap hps ->
             do let anyShared = heapIsShared i hpt
                let fn hp = do let worker ((t, FunctionNode, 0), args) = do rhs <- lookupEq (VarEntry t)
                                                                            when (anyShared && rhs /= mempty) $
                                                                              addReduced (HeapEntry hp) rhs
                                                                            return rhs
                                   worker ((t, nt, missing), args)     = return $ Other (Map.singleton (t, nt, missing) args) []
                               case lookupLhs (HeapEntry hp) hpt of
                                 Empty        -> return mempty
                                 Other{rhsTagged = nodes} -> liftM mconcat $ mapM worker (Map.toList nodes)
                liftM mconcat $ mapM fn (Set.toList hps)
           rhs -> error $ "Eval: " ++ show (rhs, i)

reduceApply a b
    = do rhs <- lookupEq (VarEntry a)
         case rhs of
          Empty -> return Empty
          Other{rhsTagged = nodes} ->
            do let f ((func, FunctionNode, 1), args)
                     = reduceEq (Ident func)
                   f ((conc, nt, n), args)
                       | n == 0    = return mempty
                       | otherwise = do bRhs <- lookupEq (VarEntry b)
                                        return $ Other (Map.singleton (conc, nt, (n-1)) (args ++ [bRhs])) []
               liftM mconcat $ mapM f (Map.toList nodes)



-- FIXME: Throw an exception if 'lhs' couldn't be found.
lookupEq :: Lhs -> M Interface.Rhs
lookupEq lhs
    = gets $ \(hpt) -> lookupLhs lhs hpt

-- FIXME: Throw an exception if 'lhs' couldn't be found.
isShared :: Lhs -> M Bool
isShared lhs
    = gets $ \(hpt) -> hptIsShared lhs hpt

setShared :: HeapPointer -> M ()
setShared hp = modify $ \hpt -> hptSetShared (HeapEntry hp) hpt

{-
lhsIsDead :: Lhs -> M Bool
lhsIsDead lhs
    = asks $ \(_hpt, dead) -> lhs `Set.member` dead

lhsSetDead :: Lhs -> M ()
lhsSetDead lhs
    = tell (mempty, Endo $ Set.insert lhs)
-}

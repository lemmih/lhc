{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, NoMonomorphismRestriction #-}
module Grin.Stage2.Optimize.Case
    ( optimize
    , destructiveOptimize
    , findRewriteRules
    , RewriteRules(..)
    , RewriteRule(..)
    , applyRewriteRules
    , inlinePass
    ) where

import Grin.Stage2.Types

import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Maybe
import Grin.Stage2.Transform

import Traverse
import Debug.Trace


optimize :: Grin -> Grin
optimize = execTrans (sequence_ [ return () -- transformExp' caseSplit
                                , return () -- transformExp caseLowering
                                , transformExp (return . promoteBottoms)
                                , runReaderT (transformExp storeFetch) Map.empty
                                ]
                     )

destructiveOptimize :: Grin -> Grin
destructiveOptimize
    = execTrans (sequence_ [ transformExp caseLowering ])

{-
do d <- case a of A -> b
                  B -> c
   e
===>
fn args = e

do case a of A -> new <- b; fn args[d->new]
             B -> new' <- c; fn args[d->new']
-}
caseSplit :: FuncDef -> Expression -> Transform Expression
caseSplit def exp 
    = case exp of
        Case scrut alts :>>= vars :-> e
          -> do e' <- hoistToTopLevel def =<< caseSplit def e
                alts' <- forM alts $ \(cond :> branch) -> do newVars <- mapM newVariableFrom vars
                                                             e'' <- renameExp (Map.fromList $ zip vars newVars) e'
                                                             return $ cond :> (branch :>>= newVars :-> e'')
                caseSplit def $ Case scrut alts'
        Case scrut alts
          -> do alts' <- forM alts $ \(cond :> branch) -> if isCheap branch then return (cond :> branch)
                                                          else do branch' <- hoistToTopLevel def =<< caseSplit def branch
                                                                  return (cond :> branch')
                return $ Case scrut alts'
        _other
          -> tmapM (caseSplit def) exp

isCheap exp = expressionSize exp < 5

expressionSize exp
    = case exp of
        Application{} -> 1
        Constant{}    -> 1
        Store{}       -> 1
        Unit{}        -> 1
        StoreHole{}   -> 1
        Case _ alts   -> sum [ expressionSize branch | cond :> branch <- alts ]
        Fetch{}       -> 1
        a :>>= _ :-> e-> expressionSize a + expressionSize e



----------------------------
-- Inlining.

data Usage
    = Once
    | Many
    | Bottom
joinUsage Bottom _ = Bottom
joinUsage _ Bottom = Bottom
joinUsage Many _ = Many
joinUsage _ Many = Many
joinUsage _ _    = Many

type FunctionUsage = Map.Map Renamed Usage

gatherFunctionUsage :: Grin -> FunctionUsage
gatherFunctionUsage grin = Map.unionsWith joinUsage (map functionUsage (grinFunctions grin))

functionUsage :: FuncDef -> FunctionUsage
--functionUsage FuncDef{funcDefName = name, funcDefBody = body}
--    | body == unreachable = Map.singleton name Bottom
functionUsage def
    = if self `Map.member` usage
      then Map.insertWith joinUsage self Many usage
      else usage
    where usage = expressionUsage (funcDefBody def)
          self  = funcDefName def

expressionUsage :: Expression -> FunctionUsage
expressionUsage exp
    = case exp of
        Application fn _args -> Map.singleton fn Once
        Constant{}           -> Map.empty
        Store{}              -> Map.empty
        Unit{}               -> Map.empty
        StoreHole{}          -> Map.empty
        Case _ alts          -> Map.unionsWith joinUsage [ expressionUsage branch | _ :> branch <- alts ]
        Fetch{}              -> Map.empty
        a :>>= _ :-> b       -> Map.unionWith joinUsage (expressionUsage a) (expressionUsage b)

inlinePass :: Grin -> Grin
inlinePass grin
    = execTrans (runReaderT (transformExp' inlineWorker) (gatherFunctionUsage grin, functionBodies)) grin
    where functionBodies = Map.fromList [ (funcDefName def, (funcDefArgs def, funcDefBody def)) | def <- grinFunctions grin ]

type Inline a = ReaderT (FunctionUsage, Map.Map Renamed ([Renamed],Expression)) Transform a

inlineWorker :: FuncDef -> Expression -> Inline Expression
inlineWorker def exp
    = do usage <- lookupFunctionUsage (funcDefName def)
         case usage of
           Many   -> inlineWorker' exp
           _other -> return exp
    where lookupFunctionUsage name = asks (Map.findWithDefault Many name . fst)

inlineWorker' :: Expression -> Inline Expression
inlineWorker' exp
    = case exp of
        Application fn args
          -> do usage <- lookupFunctionUsage fn
                case usage of
                  Many -> return $ Application fn args
                  _once -> do mbBody <- functionBody fn
                              case mbBody of
                                Nothing -> return $ Application fn args
                                Just (oldArgs, body) -> ignore fn $ inlineWorker' =<< (lift $ renameExp (Map.fromList $ zip oldArgs args) body)
        _other
          -> tmapM inlineWorker' exp
    where lookupFunctionUsage name = asks (Map.findWithDefault Many name . fst)
          ignore fn = local (\(usage,bodies) -> (Map.delete fn usage, bodies))
          functionBody name = asks (Map.lookup name . snd)



----------------------------
-- Remove unnecessary cases.
-- This removes information from the system.

caseLowering :: Expression -> Transform Expression
caseLowering exp
    = case exp of
        Case scrut [cond :> branch]
          -> caseLowering branch
        Case scrut alts
          -> tmapM caseLowering (Case scrut $ removeUnreachableBranches alts)
        _other
          -> tmapM caseLowering exp


unreachable = Application (Builtin "unreachable") []
removeUnreachableBranches alts = [ cond :> branch | cond :> branch <- alts, branch /= unreachable ]

promoteBottoms :: Expression -> Expression
promoteBottoms exp
    = case tmap promoteBottoms exp of
        a :>>= binds :-> b
          | b == unreachable || a == unreachable
          -> unreachable
        other -> other


type StoreFetch = ReaderT (Map.Map Expression Expression) Transform

storeFetch :: Expression -> StoreFetch Expression
storeFetch exp
    = case exp of
        StoreHole size :>>= vars :-> e
          -> do e' <- storeFetch e
                return $ StoreHole size :>>= vars :-> e'
        a :>>= vars :-> e
          -> do mbMatch <- asks (Map.lookup a)
                case mbMatch of
                  Nothing  -> addBinding a (Unit vars) $
                              do let extra = case a of
                                               Store _size vals -> addBindings [ (Fetch n (head vars), Unit [val]) | (n,val) <- zip [0..] vals ]
                                               _          -> id
                                 e' <- extra $ storeFetch e
                                 return $ a :>>= vars :-> e'
                  Just new -> storeFetch (new :>>= vars :-> e)
        _ -> do mbMatch <- asks (Map.lookup exp)
                case mbMatch of
                  Nothing  -> tmapM storeFetch exp
                  Just new -> return new
    where addBinding key val = local (Map.insert key val)
          addBindings [] = id
          addBindings ((k,v):xs) = addBinding k v . addBindings xs

-----------------------
-- Simple rewrite rules

data RewriteRule = RewriteRule Int Value [Renamed] Expression
type RewriteRules = Map.Map Renamed [RewriteRule]

findRewriteRules :: Grin -> RewriteRules
findRewriteRules grin = Map.fromList  (map findRewriteRule (grinFunctions grin))

findRewriteRule :: FuncDef -> (Renamed, [RewriteRule])
findRewriteRule def
    = (funcDefName def, worker 0 (funcDefBody def) )
    where worker size exp | size > 5
              = []
          worker size exp
              = case exp of
                  Case scrut alts
                      | Just idx <- argumentIndex scrut
                                  -> [RewriteRule idx cond (funcDefArgs def) (funcDefBody def) | cond :> branch <- alts ]
                  a :>>= _ :-> b  -> worker (size + expressionSize a) b
                  _other          -> []
          argumentIndex arg = lookup arg (zip (funcDefArgs def) [0..])




applyRewriteRules :: Grin -> Grin
applyRewriteRules grin = execTrans (runReaderT (transformExp apply) (scope,rules)) grin
    where rules = findRewriteRules grin
          scope = Map.empty

type Scope = Map.Map Renamed Value
type Apply a = ReaderT (Scope, RewriteRules) Transform a

apply :: Expression -> Apply Expression
apply exp
    = case exp of
        (Constant val :>>= [bind] :-> exp)
          -> addBinding bind val $
             do exp' <- apply exp
                return $ Constant val :>>= [bind] :-> exp'
        (Unit vals :>>= binds :-> exp)
          -> extendBindings (zip vals binds) $
             do exp' <- apply exp
                return $ Unit vals :>>= binds :-> exp'
        Application fn args
          -> do rules <- getRewriteRules fn
                let worker [] = return unreachable
                    worker (RewriteRule idx matchValue fnArgs newExp : rest)
                      = do mbValue <- isConstant (args!!idx)
                           case mbValue of
                             Nothing -> worker rest
                             Just value
                               -> if matchValue == value || matchValue == Empty
                                  then lift $ renameExp (Map.fromList $ zip fnArgs args) newExp
                                  else worker rest
                case rules of
                  []         -> return $ Application fn args
                  _otherwise -> return $ Application fn args -- worker rules
        Case scrut alts
          -> do alts' <- forM alts $ \(cond :> branch) -> do branch' <- addBinding scrut cond $ apply branch
                                                             return (cond :> branch')
                return $ Case scrut alts'
        _other
          -> tmapM apply exp

addBinding :: Renamed -> Value -> Apply a -> Apply a
addBinding bind val
    = local (\(scope, rules) -> (Map.insert bind val scope, rules))

isConstant :: Renamed -> Apply (Maybe Value)
isConstant name
    = asks (Map.lookup name . fst)

extendBinding :: Renamed -> Renamed -> Apply a -> Apply a
extendBinding old new fn
    = do mbValue <- isConstant old
         case mbValue of
           Nothing -> fn
           Just val -> addBinding new val fn

extendBindings :: [(Renamed, Renamed)] -> Apply a -> Apply a
extendBindings [] = id
extendBindings ((a,b):xs) = extendBinding a b . extendBindings xs

getRewriteRules :: Renamed -> Apply [RewriteRule]
getRewriteRules name
    = asks (Map.findWithDefault [] name . snd)



{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleContexts, PatternGuards #-}
module Grin.Optimize.Simple
    ( optimize
    ) where

import Grin.Types

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Traverse

type Opt a = ReaderT Subst UniqueSupply a
type Subst = Map.Map Renamed Renamed

type UniqueSupply = State Int


optimize :: Grin -> Grin
optimize grin
    = case runState (mapM simpleFuncDef (grinFunctions grin)) (grinUnique grin) of
        (funcs, unique') -> grin{ grinFunctions = funcs
                                , grinUnique = unique' }

newUnique :: MonadState Int m => m Renamed
newUnique = do u <- get
               put $! u+1
               return $ Anonymous u


simpleFuncDef :: FuncDef -> State Int FuncDef
simpleFuncDef def
    = do simplified <- runReaderT (simpleExpression (funcDefBody def)) Map.empty
         evaled     <- runReaderT (evalOpt simplified) Map.empty
         applied    <- runReaderT (applyOpt evaled) Map.empty
         fetched    <- runReaderT (fetchOpt applied) Map.empty
         pruned     <- runReaderT (casePruneOpt fetched) Map.empty
         knownCased <- runReaderT (knownCaseOpt pruned) Map.empty
         vectorOpt  <- runReaderT (vectorCaseOpt knownCased) Map.empty
         return $ def{ funcDefBody = vectorOpt }

simpleExpression :: Expression -> Opt Expression
simpleExpression (Unit (Variable v1) :>>= v2 :-> t)
    = do v1' <- doSubst v1
         subst v2 v1' (simpleExpression t)
simpleExpression (a :>>= v1 :-> Unit (Variable v2)) | v1 == v2
    = simpleExpression a
simpleExpression (Case v1 [cond :> branch] :>>= v2 :-> t)
    = simpleExpression (Case v1 [cond :> (branch :>>= v2 :-> t)])
simpleExpression ((a :>>= b :-> c) :>>= d)
    = simpleExpression (a :>>= b :-> c :>>= d)
simpleExpression ((a :>>= b :-> c) :>> d)
    = simpleExpression (a :>>= b :-> c :>> d)
simpleExpression ((a :>> b) :>>= c)
    = simpleExpression (a :>> b :>>= c)
simpleExpression ((a :>> b) :>> c)
    = simpleExpression (a :>> b :>> c)
simpleExpression (Unit Empty :>> c)
    = simpleExpression c
simpleExpression (a :>> b)
    = do a' <- simpleExpression a
         b' <- simpleExpression b
         return (a' :>> b')
simpleExpression (a :>>= b :-> c)
    = do a' <- simpleExpression a
         c' <- simpleExpression c
         return (a' :>>= b :-> c')
simpleExpression (Application fn values)
    = liftM (Application fn) $ doSubsts values
simpleExpression (Store size v)
    = liftM (Store size) $ simpleValue v
simpleExpression (Update size ptr val)
    = return (Update size) `ap` doSubst ptr `ap` doSubst val
simpleExpression (Unit value)
    = liftM Unit (simpleValue value)
simpleExpression (Case var [])
    = return $ Application (Builtin "unreachable") []
simpleExpression (Case var [Variable v :> alt])
    = simpleExpression (Unit (Variable var) :>>= v :-> alt)
simpleExpression (Case var alts) | and [ case alt of Unit ret -> ret == cond; _ -> False | cond :> alt <- alts]
    = simpleExpression (Unit (Variable var))
simpleExpression (Case val alts)
    = do val' <- doSubst val
         alts' <- mapM simpleAlt alts
         return $ Case val' alts'


simpleAlt :: Alt -> Opt Alt
simpleAlt (v :> e) = do e' <- simpleExpression e
                        return (v :> e')



simpleValue :: Value -> Opt Value
simpleValue (Variable v)
    = liftM Variable $ doSubst v
simpleValue (Node name ty missing args)
    = liftM (Node name ty missing) $ doSubsts args
simpleValue (Vector vs)
    = liftM Vector $ doSubsts vs
simpleValue v@Lit{}  = return v
simpleValue v@Hole{} = return v
simpleValue v@Empty  = return v

doSubst :: Renamed -> Opt Renamed
doSubst var
    = asks $ \m -> case Map.lookup var m of
                     Nothing     -> var
                     Just newVar -> newVar

doSubsts :: [Renamed] -> Opt [Renamed]
doSubsts = mapM doSubst

subst :: Renamed -> Renamed -> Opt a -> Opt a
subst name value = local $ Map.insert name value




-- do p <- store x 
--    y <- fetch p
--    m
--  >>>
-- do y <- unit x
--    m
type FetchOpt a = ReaderT Heap UniqueSupply a
type Heap = Map.Map (Either Renamed Renamed) Expression

fetchOpt :: Expression -> FetchOpt Expression
fetchOpt e@(Store size val :>>= bind :-> _)
    = local (Map.insert (Left bind) (Unit val))
            (tmapM fetchOpt e)
fetchOpt e@(Application (Builtin "fetch") [val] :>>= bind :-> _)
    = local (Map.insert (Right bind) (Unit (Variable val)))
            (tmapM fetchOpt e)
fetchOpt e@(Update size ptr val :>> _)
    = local (Map.insert (Left ptr) (Unit (Variable val)))
            (tmapM fetchOpt e)
fetchOpt e@(Application (Builtin "fetch") [ptr])
    = do mbVal <- asks $ Map.lookup (Left ptr)
         case mbVal of
           Nothing -> return e
           Just e' -> return e'
fetchOpt e@(Store size (Variable val))
    = do mbVal <- asks $ Map.lookup (Right val)
         case mbVal of
           Nothing -> return e
           Just e' -> return e'
fetchOpt e = tmapM fetchOpt e


type EvalOpt a = ReaderT (Map.Map Renamed Value) UniqueSupply a

evalOpt :: Expression -> EvalOpt Expression
evalOpt e@(Store size val :>>= bind :-> _)
    = local (Map.insert bind val)
            (tmapM evalOpt e)
evalOpt e@(Unit val :>>= bind :-> _)
    = local (Map.insert bind val)
            (tmapM evalOpt e)
evalOpt e@(Update size ptr val :>> _)
    = local (Map.insert ptr (Variable val))
            (tmapM evalOpt e)
evalOpt e@(Application (Builtin "eval") [ptr])
    = do mbNode <- fetchNode (Variable ptr)
         case mbNode of
           Nothing -> return e
           Just node@(Node func FunctionNode 0 args) | not (isBuiltin func)
             -> do tmp <- newUnique
                   return $ Application func args :>>= tmp :-> Application (Builtin "caseUpdate") [ptr, tmp] :>> Unit (Variable tmp)
           Just node@(Node (Builtin "evalApply") FunctionNode 0 [a,b])
             -> do a' <- newUnique
                   y <- newUnique
                   return $ Application (Builtin "eval") [a] :>>= a' :->
                            Application (Builtin "apply") [a', b] :>>= y :->
                            Application (Builtin "caseUpdate") [ptr, y] :>> Unit (Variable y)
           Just node@(Node _ FunctionNode 0 args)
             -> return e
           Just node@(Node _ ConstructorNode _ _)
             -> return $ Unit node
           Just node@(Node _ FunctionNode missingArity _) | missingArity > 0
             -> return $ Unit node
           _other
             -> return e
    where fetchNode node@Node{} = return (Just node)
          fetchNode (Variable v)
              = do mbVal <- asks $ Map.lookup v
                   case mbVal of
                     Nothing  -> return Nothing
                     Just val -> fetchNode val
          fetchNode _ = return Nothing
evalOpt e = tmapM evalOpt e


type ApplyOpt a = ReaderT (Map.Map Renamed Value) UniqueSupply a

applyOpt :: Expression -> ApplyOpt Expression
applyOpt e@(Unit val :>>= bind :-> _)
    = local (Map.insert bind val)
            (tmapM applyOpt e)
applyOpt e@(Application (Builtin "apply") [fn,arg])
    = do mbVal <- asks (Map.lookup fn)
         case mbVal of
           Just (Node _tag FunctionNode 0 _args) -> error "Grin.Optimize.Simple.applyOpt: Invalid application."
           Just (Node tag FunctionNode 1 args)
             -> return (Application tag (args ++ [arg]))
           Just (Node tag FunctionNode n args)
             -> return (Unit (Node tag FunctionNode (n-1) (args ++ [arg])))
           _ -> return e
applyOpt e = tmapM applyOpt e



type CasePruneOpt a = ReaderT Cases UniqueSupply a
type Cases = Map.Map Renamed (Set.Set Renamed)

casePruneOpt :: Expression -> CasePruneOpt Expression
casePruneOpt e@(Unit (Node tag _ _ _) :>>= v :-> _)
    = local (Map.insertWith Set.union v (Set.singleton tag))
            (tmapM casePruneOpt e)
casePruneOpt e@(Case scrut alts)
    = do mbVals <- asks $ Map.lookup scrut
         case mbVals of
           Nothing   -> tmapM casePruneOpt e
           Just vals -> let worker (Node tag _ _ _ :> _) | tag `Set.notMember` vals = Nothing
                            worker alt = Just alt
                        in tmapM casePruneOpt (Case scrut (mapMaybe worker alts))
casePruneOpt e = tmapM casePruneOpt e


type KnownCaseOpt a = ReaderT KnownCases UniqueSupply a
type KnownCases = Map.Map Renamed (Set.Set Value)

knownCaseOpt :: Expression -> KnownCaseOpt Expression
knownCaseOpt e@(Unit value :>>= v :-> _)
    = local (Map.insertWith Set.union v (Set.singleton value))
            (tmapM knownCaseOpt e)
knownCaseOpt e@(Case scrut alts)
    = do mbVals <- asks $ Map.lookup scrut
         case mbVals of
           Nothing   -> tmapM knownCaseOpt e
           Just vals -> case (Set.toList vals, alts) of
                          ([val], [cond :> branch]) | Just args <- matchValue val cond
                            -> tmapM knownCaseOpt $ foldr (\(from,to) b -> Unit (Variable from) :>>= to :-> b) branch args
                          _ -> tmapM knownCaseOpt (Case scrut alts)
    where matchValue (Node tag1 nt1 missing1 args1) (Node tag2 nt2 missing2 args2)
              | (tag1, nt1, missing1) == (tag1, nt1, missing1)
              = Just (zip args1 args2)
          matchValue _ _ = Nothing
knownCaseOpt e = tmapM knownCaseOpt e


type VectorCaseOpt a = ReaderT (Map.Map Renamed [Renamed]) UniqueSupply a

vectorCaseOpt :: Expression -> VectorCaseOpt Expression
vectorCaseOpt e@(Unit (Vector vs) :>>= v :-> _)
    = local (Map.insert v vs)
            (tmapM vectorCaseOpt e)
vectorCaseOpt e@(Case scrut [ Vector vs :> branch ])
    = do mbVals <- asks $ Map.lookup scrut
         case mbVals of
           Nothing   -> tmapM vectorCaseOpt e
           Just vals -> tmapM vectorCaseOpt (foldr (\(v,v') r -> Unit (Variable v) :>>= v' :-> r) branch (zip vals vs))
vectorCaseOpt e = tmapM vectorCaseOpt e



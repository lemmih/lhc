{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, NoMonomorphismRestriction #-}
module Grin.Stage2.Optimize.Simple
    ( optimize
    ) where

import Grin.Stage2.Types

import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Map as Map

import Traverse

type Opt a = Reader Subst a
type Subst = Map.Map Renamed Renamed


optimize :: Grin -> Grin
optimize = grinTrivialCaseCase . grinSimple

grinSimple :: Grin -> Grin
grinSimple grin
    = grin{ grinFunctions = map simpleFuncDef (grinFunctions grin)}


simpleFuncDef :: FuncDef -> FuncDef
simpleFuncDef def
    = def{ funcDefBody = runKnownCase $
                         --runConstantPropagation $
                         runSimpleExpression (funcDefBody def) }

runSimpleExpression :: Expression -> Expression
runSimpleExpression e = runReader (simpleExpression e) Map.empty

simpleExpression :: Expression -> Opt Expression
simpleExpression (Case scrut [cond :> branch] :>>= binds :-> e)
    = simpleExpression (Case scrut [cond :> branch :>>= binds :-> e])
simpleExpression (Unit v1 :>>= v2 :-> b)
    = do v1' <- doSubsts v1
         subst (zip v2 (v1' ++ repeat (Builtin "undefined"))) (simpleExpression b)
simpleExpression (e :>>= binds :-> Unit binds') | binds == binds'
    = simpleExpression e
simpleExpression (Constant c :>>= (bind:binds) :-> e)
    = do e' <- simpleExpression (Unit [] :>>= binds :-> e)
         return (Constant c :>>= [bind] :-> e')
simpleExpression (a :>>= v1 :-> Unit v2) | v1 == v2
    = simpleExpression a
simpleExpression ((a :>>= b :-> c) :>>= d)
    = simpleExpression (a :>>= b :-> c :>>= d)
simpleExpression (a :>>= b :-> c)
    = do a' <- simpleExpression a
         c' <- simpleExpression c
         return (a' :>>= b :-> c')
simpleExpression (Application fn values)
    = liftM (Application fn) $ doSubsts values
simpleExpression (StoreHole size)
    = return $ StoreHole size
simpleExpression (Store size vs)
    = liftM (Store size) $ mapM doSubst vs
simpleExpression (Unit values)
    = liftM Unit (mapM doSubst values)
simpleExpression (Case var [Empty :> alt])
    = simpleExpression alt
simpleExpression (Case var [])
    = return $ unreachable
simpleExpression (Case val alts)
    = do val' <- doSubst val
         alts' <- mapM simpleAlt alts
         return $ Case val' alts'
simpleExpression (Fetch n p)
    = liftM (Fetch n) (doSubst p)
simpleExpression (Constant c)
    = return $ Constant c

unreachable = Application (Builtin "unreachable") []

type CP a = Reader (Map.Map Value Renamed) a

runConstantPropagation :: Expression -> Expression
runConstantPropagation e = runReader (constantPropagation e) Map.empty

constantPropagation :: Expression -> CP Expression
constantPropagation (Case scrut alts)
    = liftM (Case scrut) $ forM alts $ \(cond :> branch) -> do branch' <- local (Map.insert cond scrut) (constantPropagation branch)
                                                               return (cond :> branch')
constantPropagation (Constant v)
    = do mbVar <- asks $ Map.lookup v
         return $ case mbVar of Nothing  -> Constant v
                                Just var -> Unit [var]
constantPropagation e
    = tmapM constantPropagation e


type KC a = Reader (Map.Map Renamed Value) a

runKnownCase :: Expression -> Expression
runKnownCase e = runReader (knownCase e) Map.empty

knownCase :: Expression -> KC Expression
knownCase (Case scrut alts)
    = do mbVal <- asks $ Map.lookup scrut
         case mbVal of
           Nothing -> liftM (Case scrut) $ forM alts $ \(cond :> branch) -> do branch' <- local (Map.insert scrut cond) (knownCase branch)
                                                                               return (cond :> branch')
           Just Empty -> tmapM knownCase (Case scrut alts)
           Just val -> case lookup val [ (cond,branch) | cond :> branch <- alts ] of
                         Nothing     -> if any isDefault alts
                                        then tmapM knownCase (Case scrut alts)
                                        else return unreachable
                         Just branch -> tmapM knownCase branch
knownCase e@(Constant v :>>= (bind:_) :-> _)
    = local (Map.insert bind v)
            (tmapM knownCase e)
knownCase e
    = tmapM knownCase e


simpleAlt :: Alt -> Opt Alt
simpleAlt (v :> e) = do e' <- simpleExpression e
                        return (v :> e')


doSubst var
    = asks $ \m -> case Map.lookup var m of
                     Nothing     -> var
                     Just newVar -> newVar

--doSubsts :: [Renamed] -> Opt [Renamed]
doSubsts = mapM doSubst

--subst :: [(Renamed, Renamed)] -> Opt a -> Opt a
subst pairs = local $ \m -> Map.fromList pairs `Map.union` m






type M a = ReaderT Subst (State Int) a

grinTrivialCaseCase :: Grin -> Grin
grinTrivialCaseCase grin
    = case runState (runReaderT action Map.empty) (grinUnique grin) of
        (grin, newUnique) -> grin{grinUnique = newUnique}
    where action = do defs <- mapM trivialCaseFuncDef (grinFunctions grin)
                      return grin{grinFunctions = defs}

trivialCaseFuncDef :: FuncDef -> M FuncDef
trivialCaseFuncDef def
    = do body <- trivialCaseCase (funcDefBody def)
         return def{ funcDefBody = body }


isTrivialExpression :: Expression -> Bool
isTrivialExpression Unit{} = True
isTrivialExpression Application{} = True
isTrivialExpression _ = False

{-
  [n] <- case a of
          A -> ...
          B -> ...
          C -> ...
  [i] <- case a of
          A -> ...
          B -> ...
          C -> ...
=====>
  [n,i] <- case a of
            A -> ...
            B -> ...
            C -> ...
-}
trivialCaseCase :: Expression -> M Expression
trivialCaseCase (Case scrut1 alts1 :>>= binds1 :-> Case scrut2 alts2 :>>= binds2 :-> e)
    | scrut1 == scrut2 && all (not.isDefault) alts1 && all (not.isDefault) alts2
    = do alts <- mapM (joinAlt binds1 binds2 alts2) alts1
         trivialCaseCase (Case scrut1 alts :>>= (binds1++binds2) :-> e)
trivialCaseCase (Case scrut1 alts1 :>>= binds1 :-> Case scrut2 alts2)
    | scrut1 == scrut2 && all (not.isDefault) alts1 && all (not.isDefault) alts2
    = do alts <- mapM (joinAltEnd binds1 alts2) alts1
         trivialCaseCase (Case scrut1 alts)
trivialCaseCase (Case scrut alts :>>= binds :-> e) | isTrivialExpression e
    = do alts' <- forM alts $ \(cond :> branch) -> do binds' <- replicateM (length binds) newVariable
                                                      e' <- subst (zip binds binds') (renameExp e)
                                                      return (cond :> (branch :>>= binds' :-> e'))
         trivialCaseCase (Case scrut alts')
trivialCaseCase (Application fn values)
    = liftM (Application fn) $ doSubsts values
trivialCaseCase (StoreHole size)
    = return $ StoreHole size
trivialCaseCase (Store size vs)
    = liftM (Store size) $ mapM doSubst vs
trivialCaseCase (Unit values)
    = liftM Unit (mapM doSubst values)
trivialCaseCase (Case val alts)
    = do val' <- doSubst val
         alts' <- mapM trivialCaseAlt alts
         return $ Case val' alts'
trivialCaseCase (Fetch n p)
    = liftM (Fetch n) (doSubst p)
trivialCaseCase (Constant c)
    = return $ Constant c
trivialCaseCase (e1 :>>= binds :-> e2)
    = do e1' <- trivialCaseCase e1
         e2' <- trivialCaseCase e2
         return $ e1' :>>= binds :-> e2'

trivialCaseAlt :: Alt -> M Alt
trivialCaseAlt (v :> e) = do e' <- trivialCaseCase e
                             return (v :> e')


renameExp :: Expression -> M Expression
renameExp (Application fn values)
    = liftM (Application fn) $ doSubsts values
renameExp (StoreHole size)
    = return $ StoreHole size
renameExp (Store size vs)
    = liftM (Store size) $ mapM doSubst vs
renameExp (Unit values)
    = liftM Unit (mapM doSubst values)
renameExp (Case val alts)
    = do val' <- doSubst val
         alts' <- mapM renameAlt alts
         return $ Case val' alts'
renameExp (Fetch n p)
    = liftM (Fetch n) (doSubst p)
renameExp (Constant c)
    = return $ Constant c
renameExp (e1 :>>= binds :-> e2)
    = do e1' <- trivialCaseCase e1
         binds' <- replicateM (length binds) newVariable
         e2' <- subst (zip binds binds') (trivialCaseCase e2)
         return $ e1' :>>= binds' :-> e2'


renameAlt :: Alt -> M Alt
renameAlt (v :> e) = do e' <- renameExp e
                        return (v :> e')

{-
A -> m a
=====>
A -> do [n] <- m a
        [i] <- m b
        unit [n,i]
-}
joinAlt binds1 binds2 branches (cond :> branch)
    = do binds1' <- replicateM (length binds1) newVariable
         binds2' <- replicateM (length binds2) newVariable
         let newBranch = findBranch branches
         exp' <- subst (zip binds1 binds1') (renameExp newBranch)
         return (cond :> (branch :>>= binds1' :-> exp' :>>= binds2' :-> Unit (binds1'++binds2')))
    where findBranch [] = unreachable
          findBranch ((c :> branch):xs) | c == cond = branch
                                        | otherwise = findBranch xs

joinAltEnd binds1 branches (cond :> branch)
    = do binds1' <- replicateM (length binds1) newVariable
         let newBranch = findBranch branches
         exp' <- subst (zip binds1 binds1') (renameExp newBranch)
         return (cond :> (branch :>>= binds1' :-> exp'))
    where findBranch [] = unreachable
          findBranch ((c :> branch):xs) | c == cond = branch
                                        | otherwise = findBranch xs

isDefault (Empty :> _) = True
isDefault x = False

newVariable :: M Renamed
newVariable
    = do uid <- newUnique
         return $ Anonymous uid

newUnique :: M Int
newUnique
    = do uid <- get
         put (uid+1)
         return uid


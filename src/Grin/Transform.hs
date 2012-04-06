{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Grin.Transform
    ( Transform
    , TState(stateGrin)
    , newVariable
    , newVariableFrom
    , runTrans
    , transformExp
    , renameExp
    , freeVariables
    , pushFuncDef
    , hoistToTopLevel
    ) where

import Grin.Types
import Traverse

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set

data TState = TState { stateGrin :: !Grin }

newtype Transform a = Transform { unTransform :: State TState a }
    deriving (Monad, MonadState TState)


newVariable :: MonadState TState m => m Renamed
newVariable = do st <- get
                 let grin = stateGrin st
                 put $! st { stateGrin = grin{ grinUnique = grinUnique grin + 1 } }
                 return $ Anonymous (grinUnique grin)

newVariableFrom :: MonadState TState m => Renamed -> m Renamed
newVariableFrom oldName
    = liftM (mergeNames oldName) newVariable
    where mergeNames (Aliased _ name) (Anonymous uid) = Aliased uid name
          mergeNames _oldName newName = newName

pushFuncDef :: MonadState TState m => FuncDef -> m ()
pushFuncDef def
    = do st <- get
         let grin = stateGrin st
         put $! st { stateGrin = grin{ grinFunctions = def : grinFunctions grin  } }

runTrans :: Transform a -> Grin -> Grin
runTrans action grin
    = case execState (unTransform action) (TState grin) of
        tstate -> stateGrin tstate

transformExp :: MonadState TState m => (Expression -> m Expression) -> m ()
transformExp fn
    = do funcs <- gets (grinFunctions . stateGrin)
         modify $ \(TState grin) -> TState (grin{grinFunctions = []})
         defs <- mapM (transformFunc fn) funcs
         modify $ \(TState grin) -> TState (grin{grinFunctions = defs ++ grinFunctions grin })


transformFunc :: MonadState TState m => (Expression -> m Expression) -> FuncDef -> m FuncDef
transformFunc fn def
    = do body <- fn (funcDefBody def)
         return def{funcDefBody = body}



-- Hoist an expression to a new top-level function.
-- The returned expression calls the new function.
hoistToTopLevel :: Renamed -> Expression -> Transform Expression
hoistToTopLevel oldName exp
    = do newName <- newVariableFrom oldName
         cafs <- gets (map cafName . grinCAFs . stateGrin)
         let unboundArgs = Set.toList (free `Set.difference` Set.fromList cafs)
         args <- mapM newVariableFrom unboundArgs
         body <- renameExp (Map.fromList (zip unboundArgs args)) exp
         let funcDef = FuncDef { funcDefName = newName
                               , funcDefArgs = args
                               , funcDefBody = body }
         pushFuncDef funcDef
         return $ Application newName unboundArgs
    where free = freeVariables exp

freeVariables :: Expression -> Set.Set Renamed
freeVariables = worker
    where worker (Case scrut alts) = Set.unions (Set.singleton scrut : map freeAlt alts)
          worker (Store size val) = freeValue val
          worker (Update _size ptr val) = Set.fromList [ptr, val]
          worker (Unit val) = freeValue val
          worker (Application fn args)
              = Set.fromList args
          worker (a :>>= val :-> b)
              = Set.unions [ worker a
                           , worker b `Set.difference` Set.singleton val ]
          worker (a :>> b)
              = worker a `Set.union` worker b
          freeAlt (val :> exp) = worker exp `Set.difference` freeValue val
          freeValue (Node _node _nt _missing args) = Set.fromList args
          freeValue (Vector args) = Set.fromList args
          freeValue Lit{} = Set.empty
          freeValue (Variable v) = Set.singleton v
          freeValue Hole{} = Set.empty
          freeValue Empty = Set.empty


type Rename = ReaderT (Map.Map Renamed Renamed) Transform

renameExp :: Map.Map Renamed Renamed -> Expression -> Transform Expression
renameExp m exp = runReaderT (renameExp' exp) m

renameExp' :: Expression -> Rename Expression
renameExp' (e1 :>>= bind :-> e2)
    = bindArgument bind $ \bind' ->
      tmapM renameExp' (e1 :>>= bind' :-> e2)
renameExp' (e1 :>> e2)
    = liftM2 (:>>) (renameExp' e1) (renameExp' e2)
renameExp' (Case scrut alts)
    = do scrut' <- rename scrut
         Case scrut' <$> mapM renameAlt alts
renameExp' (Store size v)
    = renameValue (Store size) v
renameExp' (Unit v)
    = renameValue Unit v
renameExp' (Application fn args)
    = Application fn <$> mapM rename args
renameExp' (Update size ptr val)
    = return (Update size) `ap` rename ptr `ap` rename val

renameAlt (Node tag nt missing args :> branch)
    = bindArguments args $ \args' ->
      (Node tag nt missing args' :>) <$> renameExp' branch
renameAlt (Vector args :> branch)
    = bindArguments args $ \args' ->
      (Vector args' :>) <$> renameExp' branch
renameAlt (Variable v :> branch)
    = bindArgument v $ \v' ->
      (Variable v' :>) <$> renameExp' branch
renameAlt (cond :> branch)
    = (cond :>) <$> renameExp' branch

bindArgument arg fn
    = do arg' <- newVariable
         local (Map.insert arg arg') $ fn arg'

bindArguments [] fn = fn []
bindArguments (x:xs) fn = bindArgument x $ \x' -> bindArguments xs $ \xs' -> fn (x':xs')

rename :: Renamed -> Rename Renamed
rename val = asks $ Map.findWithDefault val val

renameValue fn (Variable v)
    = renameArgs [v] $ \[v'] -> fn (Variable v')
renameValue fn (Node tag nt missing args)
    = renameArgs args $ \args' -> fn (Node tag nt missing args')
renameValue fn (Vector args)
    = renameArgs args $ \args' -> fn (Vector args')
renameValue fn v
    = return $ fn v

renameArgs args fn
    = do m <- ask
         let worker acc []     = return (fn (reverse acc))
             worker acc (x:xs) = case Map.lookup x m of
                                    Nothing  -> worker (x:acc) xs
                                    Just n   -> worker (n:acc) xs
         worker [] args


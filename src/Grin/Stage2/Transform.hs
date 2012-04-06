{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Grin.Stage2.Transform
    ( Transform
    , newVariable
    , newVariableFrom
    , runTrans
    , execTrans
    , evalTrans
    , transformExp
    , transformExp'
    , renameExp
    , hoistToTopLevel
    , hoistToTopLevel'
    , freeVariables
    ) where

import Grin.Stage2.Types
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

execTrans :: Transform a -> Grin -> Grin
execTrans action grin
    = case execState (unTransform action) (TState grin) of
        tstate -> stateGrin tstate

runTrans :: Transform a -> Grin -> (a, Grin)
runTrans action grin
    = case runState (unTransform action) (TState grin) of
        (a, tstate) -> (a, stateGrin tstate)

evalTrans :: Transform a -> Grin -> a
evalTrans action grin
    = evalState (unTransform action) (TState grin)

transformExp :: MonadState TState m => (Expression -> m Expression) -> m ()
transformExp fn
    = transformExp' (const fn)

transformExp' :: MonadState TState m => (FuncDef -> Expression -> m Expression) -> m ()
transformExp' fn
    = do funcs <- gets (grinFunctions . stateGrin)
         modify $ \(TState grin) -> TState (grin{grinFunctions = []})
         defs <- mapM (transformFunc fn) funcs
         modify $ \(TState grin) -> TState (grin{grinFunctions = defs ++ grinFunctions grin })


transformFunc :: MonadState TState m => (FuncDef -> Expression -> m Expression) -> FuncDef -> m FuncDef
transformFunc fn def
    = do body <- fn def (funcDefBody def)
         return def{funcDefBody = body}


-- Hoist an expression to a new top-level function.
-- The returned expression calls the new function.
hoistToTopLevel :: FuncDef -> Expression -> Transform Expression
hoistToTopLevel oldFunction exp
    = do (funcDef, newName, unboundArgs) <- hoistToTopLevel' oldFunction exp
         pushFuncDef funcDef
         return $ Application newName unboundArgs

hoistToTopLevel' :: FuncDef -> Expression -> Transform (FuncDef, Renamed, [Renamed])
hoistToTopLevel' oldFunction exp
    = do newName <- newVariableFrom (funcDefName oldFunction)
         cafs <- gets (map cafName . grinCAFs . stateGrin)
         let unboundArgs = Set.toList (free `Set.difference` Set.fromList cafs)
         args <- mapM newVariableFrom unboundArgs
         body <- renameExp (Map.fromList (zip unboundArgs args)) exp
         let funcDef = FuncDef { funcDefName = newName
                               , funcDefArgs = args
                               , funcDefBody = body
                               , funcDefReturns = funcDefReturns oldFunction }
         return (funcDef, newName, unboundArgs)
    where free = freeVariables exp


freeVariables :: Expression -> Set.Set Renamed
freeVariables = worker
    where worker (Case scrut alts) = Set.unions (Set.singleton scrut : map freeAlt alts)
          worker (Fetch nth var) = Set.singleton var
          worker (Store size vals) = Set.fromList vals
          worker StoreHole{} = Set.empty
          worker (Unit vals) = Set.fromList vals
          worker (Application fn args)
              = Set.fromList args
          worker Constant{} = Set.empty
          worker (a :>>= vals :-> b)
              = Set.unions [ worker a
                           , worker b `Set.difference` Set.fromList vals ]
          freeAlt (val :> exp) = worker exp


type Rename = ReaderT (Map.Map Renamed Renamed) Transform

renameExp :: Map.Map Renamed Renamed -> Expression -> Transform Expression
renameExp m exp = runReaderT (renameExp' exp) m

renameExp' :: Expression -> Rename Expression
renameExp' (e1 :>>= binds :-> e2)
    = bindArguments binds $ \binds' ->
      tmapM renameExp' (e1 :>>= binds' :-> e2)
renameExp' (Case scrut alts)
    = do scrut' <- rename scrut
         Case scrut' <$> mapM renameAlt alts
renameExp' (Store size vs)
    = Store size <$> mapM rename vs
renameExp' (Fetch nth var)
    = Fetch nth <$> rename var
renameExp' (Unit vs)
    = Unit <$> mapM rename vs
renameExp' (Application fn args)
    = Application fn <$> mapM rename args
renameExp' e@Constant{}
    = return e
renameExp' e@StoreHole{}
    = return e


renameAlt (cond :> branch)
    = (cond :>) <$> renameExp' branch

bindArgument arg fn
    = do arg' <- newVariable
         local (Map.insert arg arg') $ fn arg'

bindArguments [] fn = fn []
bindArguments (x:xs) fn = bindArgument x $ \x' -> bindArguments xs $ \xs' -> fn (x':xs')

rename :: Renamed -> Rename Renamed
rename val = asks $ Map.findWithDefault val val


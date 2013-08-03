{-# LANGUAGE OverloadedStrings #-}
module Grin.Stage3.Optimize.Simple where

import Grin.Stage3.Types
import Grin.Stage3.Transform

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Traverse

optimize :: Grin -> Grin
optimize grin = cse $ execTrans (do runReaderT (transformExp simplify) Map.empty
                                    transformExp caseElimination
                                    runReaderT (transformExp casePrune) Map.empty
                                    transformExp (return . promoteBottoms bottomFuncs)
                                ) grin
    where bottomFuncs = Set.fromList [ funcDefName func | func <- grinFunctions grin, funcDefIsBottom func ]



type Opt = ReaderT (Map.Map Renamed Renamed) Transform

simplify :: Expression -> Opt Expression
simplify exp
    = case exp of
        simple :>>= binds :-> rest
          -> do simple' <- simplifyS simple
                case simple' of
                  Unit values   -> bindMany (zip binds (values ++ repeat undefinedVar)) $ simplify rest
                  Application{} -> do rest' <- simplify rest
                                      return (simple' :>>= binds :-> rest')
                  _             -> do rest' <- bindMany (zip (drop 1 binds) (repeat undefinedVar)) $ simplify rest
                                      return (simple' :>>= take 1 binds :-> rest')
        Case scrut []    -> return $ Singleton $ Application (Builtin "unreachable") []
        Case scrut [ Empty :> branch ]
                         -> simplify (Singleton branch)
        Case scrut [ node@Node{} :> branch ]
                         -> do v <- newVariableFrom scrut
                               bind scrut v $ simplify (Constant node :>>= [v] :-> Singleton branch )
        Case scrut alts  -> liftM2 Case (renameArg scrut) (mapM renameAlt alts)
        Singleton simple -> liftM Singleton $ simplifyS simple
    where undefinedVar = Builtin ("undefined")

simplifyS :: SimpleExpression -> Opt SimpleExpression
simplifyS exp
    = case exp of
        Application fn args -> Application fn <$> renameArgs args
        Fetch idx ptr       -> Fetch idx <$> renameArg ptr
        Store size values   -> Store size <$> renameArgs values
        StoreHole size      -> return (StoreHole size)
        Unit values         -> Unit <$> renameArgs values
        Constant value      -> return (Constant value)

renameAlt :: Alt -> Opt Alt
renameAlt (cond :> simple)
    = do branch <- simplifyS simple
         return $ cond :> branch

renameArg :: Renamed -> Opt Renamed
renameArg val
    = asks $ Map.findWithDefault val val

renameArgs :: [Renamed] -> Opt [Renamed]
renameArgs = mapM renameArg

bind :: Renamed -> Renamed -> Opt a -> Opt a
bind key val = local (Map.insert key val)

bindMany :: [(Renamed,Renamed)] -> Opt a -> Opt a
bindMany [] = id
bindMany ((key,val):xs) = bindMany xs . bind key val



promoteBottoms bottomFuncs exp
    = case exp of
        simple :>>= binds :-> rest
          -> let rest' = promoteBottoms bottomFuncs rest
                 simple' = worker simple in
             if isBottom (Singleton simple') || isBottom rest'
                then Singleton bottom
                else simple' :>>= binds :-> rest'
        Case scrut alts
          -> case [ cond :> worker simple | cond :> simple <- alts, simple /= bottom ] of
               []    -> Singleton bottom
               alts' -> Case scrut alts'
        Singleton simple
          -> Singleton (worker simple)
    where worker (Application fnName args) | fnName `Set.member` bottomFuncs = bottom
          worker simpl = simpl


funcDefIsBottom = isBottom . funcDefBody

isBottom (Singleton (Application (Builtin "unreachable") [])) = True
isBottom _ = False

bottom = Application (Builtin "unreachable") []        




cse :: Grin -> Grin
cse grin
    = grin { grinFunctions = map worker (grinFunctions grin) }
    where worker func = func{ funcDefBody = runReader (cseExp (funcDefBody func)) Map.empty }
type CSE = Reader (Map.Map SimpleExpression Renamed)

cseExp :: Expression -> CSE Expression
cseExp exp
    = case exp of
        simple :>>= [bind] :-> rest
          -> do known <- ask
                case Map.lookup simple known of
                  Nothing  -> local (Map.insert simple bind) $ tmapM cseExp exp
                  Just var -> tmapM cseExp (Unit [var] :>>= [bind] :-> rest)
        _ -> tmapM cseExp exp



type Prune = ReaderT (Map.Map Renamed Value) Transform
casePrune :: Expression -> Prune Expression
casePrune exp
    = case exp of
        Singleton{}        -> return exp
        Case scrut alts
          -> do mbValue <- asks (Map.lookup scrut)
                case mbValue of
                  Nothing    -> return exp
                  Just value -> case [ branch | cond :> branch <- alts, matches value cond ] of
                                  []     -> return (Singleton bottom)
                                  [x]    -> return (Singleton x)
                                  _other -> return exp
        Constant value :>>= [bind] :-> e
          -> local (Map.insert bind value) $ tmapM casePrune exp
        _ -> tmapM casePrune exp
    where matches _ Empty = True
          matches a b     = a == b

caseElimination :: Expression -> Transform Expression
caseElimination exp
    = case isSingleCase exp of
        Nothing            -> return exp
        Just (scrut, node) -> worker scrut node exp
    where worker scrut node exp
              = case exp of
                  Singleton{} -> return exp
                  Case{}      -> return exp
                  s :>>= binds :-> e
                      | scrut `elem` binds -> do v <- newVariableFrom scrut
                                                 e' <- runReaderT (simplify e) (Map.singleton scrut v)
                                                 return $ s :>>= binds :-> Constant node :>>= [v] :-> e'
                      | otherwise          -> tmapM (worker scrut node) exp
          isSingleCase exp
              = case exp of
                  Singleton{}                -> Nothing
                  Case scrut [ node@Node{} :> _branch ] -> Just (scrut, node)
                  Case{}                     -> Nothing
                  _ :>>= _ :-> e             -> isSingleCase e



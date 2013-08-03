{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ParallelListComp #-}
module Grin.Optimize.CallPattern
    ( optimize
    ) where

import Grin.Types

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Traverse
import Grin.Transform
import Debug.Trace

optimize :: Grin -> Grin
optimize = specialize



data Pattern = Pattern { patternTag :: Renamed
                       , patternType :: NodeType
                       , patternMissingArity :: Int
                       , patternArity :: Int
                       } deriving (Eq,Ord, Show)
data CallPattern = CallPattern Renamed [Maybe Pattern] deriving (Eq, Ord, Show)

type Env = Map.Map Renamed Pattern
type Rewrite = Map.Map CallPattern Renamed
type Expand = Map.Map Renamed [Renamed]
newtype M a = M { runM :: Rewrite -> Expand -> Env -> (a, Set.Set CallPattern) }

instance Monad M where
    return a = M $ \_ _ _ -> (a, mempty)
    f >>= g  = M $ \rewrite expand env ->
                   let (a, m)  = runM f rewrite expand env
                       (b, m') = runM (g a) rewrite expand env
                   in (b, m `mappend` m')

instance MonadWriter (Set.Set CallPattern) M where
    tell set      = M $ \_ _ _ -> ((), set)
    listen action = M $ \rewrite expand env -> (runM action rewrite expand env, mempty)
    pass action   = M $ \rewrite expand env -> let ((a, fn), m) = runM action rewrite expand env
                                               in (a, fn m)




---------------------------------------------------------------------------------
--

specialize :: Grin -> Grin
specialize grin
    = let grin'' = runTrans action grin'{grinUnique = grinUnique grin' + nPatterns}
          (grin''', _) = markReplaceAll (Map.fromList rewriteList) grin''
      in {-trace ("CallPatterns: " ++ show callPatterns) -} grin'''
    where (grin', callPatterns) = markReplaceAll (Map.fromList rewriteList) grin
          nPatterns = Set.size callPatterns
          rewriteList = [ (callPattern, deriveRenamed n tag)
                          | n <- [grinUnique grin .. ]
                          | callPattern@(CallPattern tag _patterns) <- Set.toList callPatterns ]
          functionMap = Map.fromList [ (funcDefName func, func) | func <- grinFunctions grin ]
          action = forM rewriteList $ \(CallPattern funcName patterns, name) ->
                   case Map.lookup funcName functionMap of
                     Nothing   -> return () -- Probably a builtin or primitive.
                     Just func -> specializeCallPattern name patterns func

specializeCallPattern :: Renamed -> [Maybe Pattern] -> FuncDef -> Transform ()
specializeCallPattern fnName patterns func
    = do (body, oldArgs) <- worker (funcDefBody func) patterns (funcDefArgs func) []
         cafs <- gets (map cafName . grinCAFs . stateGrin)
         let free = freeVariables body
         args <- mapM newVariableFrom oldArgs
         newBody <- renameExp (Map.fromList (zip oldArgs args)) body
         let funcDef = FuncDef { funcDefName = fnName
                               , funcDefArgs = args
                               , funcDefBody = newBody }
         pushFuncDef funcDef
    where worker body [] [] newArgs = return (body, newArgs)
          worker body (mbPattern:patterns) (arg:args) newArgs
              = case mbPattern of
                  Nothing      -> do worker body patterns args (newArgs ++ [arg])
                  Just pattern -> do vs <- replicateM (patternArity pattern) newVariable
                                     let newBody = Store 0 (Node (patternTag pattern) (patternType pattern) (patternMissingArity pattern) vs) :>>= arg :-> body
                                     worker newBody patterns args (newArgs ++ vs)




---------------------------------------------------------------------------------
--

markReplaceAll :: Rewrite -> Grin -> (Grin, Set.Set CallPattern)
markReplaceAll rewrite grin
    = case runM (mapM markReplaceFunc (grinFunctions grin)) rewrite Map.empty Map.empty of
        (funcs, callPatterns) -> (grin{grinFunctions = funcs}, Set.filter interestingPattern callPatterns)
    where interestingPattern (CallPattern tag [])   = True
          interestingPattern (CallPattern tag args) = any isJust args

markReplaceFunc :: FuncDef -> M FuncDef
markReplaceFunc func
    = do body <- markReplacePatterns (funcDefBody func)
         return func{ funcDefBody = body }

markReplacePatterns :: Expression -> M Expression
markReplacePatterns exp
    = case exp of
       Application fn args | not (isBuiltin fn)
                      -> do callPattern <- mkCallPattern fn args
                            markCallPattern callPattern
                            rewriteApplication callPattern args
       Store size value  -> liftM (Store size) $ rewriteValue value
       Unit value     -> liftM Unit $ rewriteValue value
       a :>>= v :-> b -> worker a v $ tmapM markReplacePatterns exp
       _other         -> tmapM markReplacePatterns exp
    where worker node variable action
              = case node of
                  Store _size (Node tag FunctionNode missingArity args) | missingArity > 0
                    -> let pattern = Pattern { patternTag = tag
                                             , patternType = FunctionNode
                                             , patternMissingArity = missingArity
                                             , patternArity = length args }
                       in expand variable args $ bind variable pattern $ action
                  Unit (Node tag FunctionNode missingArity args) | missingArity > 0
                    -> let pattern = Pattern { patternTag = tag
                                             , patternType = FunctionNode
                                             , patternMissingArity = missingArity
                                             , patternArity = length args }
                       in expand variable args $ bind variable pattern $ action
                  Store _size (Variable v)
                    -> do expanded <- expandArg v
                          mbPattern <- lookupPattern v
                          case mbPattern of
                            Nothing      -> action
                            Just pattern -> expand variable expanded $ bind variable pattern $ action
                  _other -> action


rewriteValue :: Value -> M Value
rewriteValue (Node tag FunctionNode missingArity args) | not (isBuiltin tag)
    = do callPattern <- mkCallPattern tag (args ++ replicate missingArity (Builtin "undefined"))
         markCallPattern callPattern
         rewrite <- askRewrite
         expandedArgs <- expandArgs args
         let result = case Map.lookup callPattern rewrite of
                        Nothing    -> Node tag FunctionNode missingArity args
                        Just newFn -> Node newFn FunctionNode missingArity expandedArgs
         return result
rewriteValue value = return value

rewriteApplication :: CallPattern -> [Renamed] -> M Expression
rewriteApplication callPattern@(CallPattern fn _patterns) args
    = do rewrite <- askRewrite
         expandedArgs <- expandArgs args
         let result = case Map.lookup callPattern rewrite of
                        Nothing    -> Application fn args
                        Just newFn -> Application newFn expandedArgs
         return result



-- Create a 'call pattern' from a function name and a list of arguments.
mkCallPattern :: Renamed -> [Renamed] -> M CallPattern
mkCallPattern fn args
    = do patterns <- mapM lookupPattern args
         return $ CallPattern fn patterns

lookupPattern :: Renamed -> M (Maybe Pattern)
lookupPattern variable
    = do env <- askEnv
         return $ Map.lookup variable env

markCallPattern :: CallPattern -> M ()
markCallPattern = tell . Set.singleton

askEnv :: M Env
askEnv = M $ \_rewrite _expand env -> (env, mempty)
              
askRewrite :: M Rewrite
askRewrite = M $ \rewrite _expand _env -> (rewrite, mempty)

bind :: Renamed -> Pattern -> M a -> M a
bind from to action
    = M $ \_rewrite _expand env -> runM action _rewrite _expand (Map.insert from to env)

expand :: Renamed -> [Renamed] -> M a -> M a
expand from to action
    = M $ \_rewrite expand _env -> runM action _rewrite (Map.insert from to expand) _env

expandArg :: Renamed -> M [Renamed]
expandArg variable
    = M $ \_rewrite expand _env -> (Map.findWithDefault [variable] variable expand, mempty)

expandArgs :: [Renamed] -> M [Renamed]
expandArgs = liftM concat . mapM expandArg

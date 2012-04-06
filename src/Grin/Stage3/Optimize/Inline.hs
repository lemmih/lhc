{-# LANGUAGE OverloadedStrings #-}
module Grin.Stage3.Optimize.Inline
    ( inline
    ) where

import Grin.Stage3.Types
import Grin.Stage3.Transform

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Traverse

import Debug.Trace

inline :: Grin -> Grin
inline = runCaseShortcut . inlineCheap . inlineOneShot


----------------------------------------------
-- Inline very cheap functions.

inlineCheap :: Grin -> Grin
inlineCheap grin
    = execTrans (runReaderT (transformExp inlineExp) funcMap) grin
    where funcMap = Map.fromList [ (funcDefName def, def) | def <- grinFunctions grin
                                 , functionType def == Cheap ]


-----------------------------------------
-- Inline functions that are used once.


analyzeOneShot :: Grin -> Map.Map Renamed Int
analyzeOneShot grin
    = Map.unionsWith (+) (map analyzeFuncDef (grinFunctions grin))
    where analyzeFuncDef = analyzeExpression . funcDefBody
          analyzeExpression exp
              = case exp of
                  simple :>>= binds :-> rest -> Map.unionWith (+) (analyzeSimple simple) (analyzeExpression rest)
                  Case scrut alts            -> Map.unionsWith (+) [ analyzeSimple simple | _ :> simple <- alts ]
                  Singleton simple           -> analyzeSimple simple
          analyzeSimple simple
              = case simple of
                  Application name args      -> Map.singleton name 1
                  Fetch{}                    -> Map.empty
                  Store{}                    -> Map.empty
                  StoreHole{}                -> Map.empty
                  Unit{}                     -> Map.empty
                  Constant{}                 -> Map.empty

inlineOneShot :: Grin -> Grin
inlineOneShot grin
    = execTrans (runReaderT (transformExp inlineExp) funcMap) grin
    where analysis = analyzeOneShot grin
          funcMap = Map.fromList [ (funcDefName def, def) | def <- grinFunctions grin
                                 , Just 1 <- [Map.lookup (funcDefName def) analysis] ]


----------------------------------
--

data UsePattern = UsePattern [Renamed] Int [ (Value, Expression) ]

findUsePatterns :: Grin -> Map.Map Renamed UsePattern
findUsePatterns grin
    = Map.fromList [ (funcDefName func, usePattern) | func <- grinFunctions grin,  Just usePattern <- [funcDefUsePattern func] ]

funcDefUsePattern :: FuncDef -> Maybe UsePattern
funcDefUsePattern func
    = worker id (funcDefBody func)
    where worker acc exp
              = case exp of
                  Singleton simple
                    -> Nothing
                  Case scrut alts -> do n <- elemIndex scrut (funcDefArgs func)
                                        return $ UsePattern (funcDefArgs func) n [ (cond, acc (Singleton branch)) | cond :> branch <- alts ]
                  simple :>>= binds :-> rest
                    -> worker (\rest' -> acc (simple :>>= binds :-> rest')) rest


runCaseShortcut :: Grin -> Grin
runCaseShortcut grin
    = execTrans (runReaderT (transformExp caseShortcut) emptyEnv) grin
    where emptyEnv = ShortcutEnv { shortcutConstants   = Map.empty
                                 , shortcutUsePatterns = findUsePatterns grin }

data ShortcutEnv = ShortcutEnv { shortcutConstants :: Map.Map Renamed Value
                               , shortcutUsePatterns :: Map.Map Renamed UsePattern
                               }
type Shortcut = ReaderT ShortcutEnv Transform

bindVariable :: Renamed -> Value -> Shortcut a -> Shortcut a
bindVariable key Empty = id
bindVariable key value
    = local (\env -> env{ shortcutConstants = Map.insert key value (shortcutConstants env) })

caseShortcut exp
    = case exp of
        Singleton (Application fnName args)
          -> do (append, newInst) <- process (const True) fnName args
                tmapM caseShortcut (append (Singleton newInst))
        Case scrut alts
          -> do r <- forM alts $ \alt@(cond :> simple)
                        -> bindVariable scrut cond $
                           case simple of
                             Application fnName args
                               -> do (append, newBranch) <- process isNonStrict fnName args
                                     return (cond :> newBranch, append)
                             _ -> return (alt, id)
                let alts' = map fst r
                    fn = foldr (.) id (map snd r)
                tmapM caseShortcut $ fn (Case scrut alts')
        Application fnName args :>>= binds :-> rest
          -> do (append, newInst) <- process (const True) fnName args
                tmapM caseShortcut $ append (newInst :>>= binds :-> rest)
        Constant value :>>= [bind] :-> rest
          -> bindVariable bind value $
             tmapM caseShortcut exp
        _ -> tmapM caseShortcut exp
    where process guard fnName args
              = do ShortcutEnv consts usePatterns <- ask
                   case do UsePattern funcArgs n alts <- Map.lookup fnName usePatterns
                           value <- Map.lookup (args!!n) consts
                           let newExp = matchValue value alts
                           return (newExp, funcArgs)
                     of Just (newExp, funcArgs) | guard newExp
                          -> do renamedExp <- lift $ renameExp (Map.fromList $ zip funcArgs args) newExp
                                case expressionType renamedExp of
                                  TailCall append simple -> return (append, simple)
                                  _                      -> return (id, unreachable)
                        _otherwise      -> return (id, Application fnName args)

matchValue :: Value -> [(Value, Expression)] -> Expression
matchValue value []     = Singleton unreachable
matchValue value ((cond,branch):xs)
    | value `matches` cond = branch
    | otherwise            = matchValue value xs
    where x `matches` Empty = True
          x `matches` y = x == y

unreachable = Application (Builtin "unreachable") []


-- Utilities

type Inline = ReaderT (Map.Map Renamed FuncDef) Transform

inlineExp :: Expression -> Inline Expression
inlineExp exp
    = case exp of
        Singleton (Application fnName args)
          -> do mbEntry <- asks $ Map.lookup fnName
                case mbEntry of
                  Nothing   -> return exp
                  Just func -> doInline func args
        Case scrut alts
          -> do r <- forM alts $ \alt@(cond :> simple)
                       -> case simple of
                            Application fnName args
                              -> do mbEntry <- asks $ Map.lookup fnName
                                    case mbEntry of
                                      Just func | isNonStrict (funcDefBody func)
                                        -> do exp <- doInline func args
                                              case expressionType exp of
                                                TailCall append simple
                                                  -> return (cond :> simple, append)
                                                _ -> return (alt, id)
                                      _otherwise -> return (alt, id)
                            _otherwise -> return (alt, id)
                let alts' = map fst r
                    fn = foldr (.) id (map snd r)
                tmapM inlineExp $ fn (Case scrut alts')
        Application fnName args :>>= binds :-> rest
          -> do mbEntry <- asks $ Map.lookup fnName
                case mbEntry of
                  Nothing   -> tmapM inlineExp exp
                  Just func -> do newExp <- doInline func args
                                  case expressionType newExp of
                                    TailCall append simple -> tmapM inlineExp $ append (simple :>>= binds :-> rest)
                                    _other                 -> tmapM inlineExp exp
        _other -> tmapM inlineExp exp




doInline func args
    = do let renamedArgs = Map.fromList (zip (funcDefArgs func) (args ++ repeat (Builtin "undefined")))
         lift (renameExp renamedArgs (funcDefBody func))




data ExprType = TailCall (Expression -> Expression) SimpleExpression
              | BranchCall

expressionType :: Expression -> ExprType
expressionType
    = worker id
    where worker acc exp
              = case exp of
                  Singleton simple                    -> TailCall acc simple
                  Case{}                              -> BranchCall
                  simple :>>= binds :-> rest          -> worker (\rest' -> acc (simple :>>= binds :-> rest')) rest

isNonStrict :: Expression -> Bool
isNonStrict exp
    = case exp of
        Singleton simple       -> True
        Case{}                 -> True
        simple :>>= _ :-> rest -> worker simple && isNonStrict rest
    where worker exp
              = case exp of
                  Application (Builtin "update") _ -> False
                  Application fnName _ -> False -- isBuiltin fnName
                  Fetch{}     -> False -- True
                  Store{}     -> False --True
                  StoreHole{} -> False -- True
                  Unit{}      -> True
                  Constant{}  -> True

data FunctionType = Cheap | Expensive
    deriving (Eq)

functionType :: FuncDef -> FunctionType
functionType
    = worker . funcDefBody
    where worker (Singleton{}) = Cheap
          worker (Case _scrut [_alternative]) = Cheap
          worker _             = Expensive

functionSize :: FuncDef -> Int
functionSize
    = worker . funcDefBody
    where worker exp
              = case exp of
                  Singleton{}       -> 1
                  Case scrut alts   -> length alts
                  _ :>>= _ :-> rest -> 1 + worker rest

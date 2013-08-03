{-# LANGUAGE OverloadedStrings #-}
module Grin.Optimize.Inline
    ( inlinePass
    ) where

import Grin.Types
import Traverse
import Grin.Transform

import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid
import Debug.Trace
import Text.Printf
import Control.Applicative

import qualified Data.Map as Map

inlinePass :: Grin -> Grin
inlinePass = inlineSimple . inlineOneShot . inlineCAFs

-- Lower cheap CAFs to regular functions.
inlineCAFs :: Grin -> Grin
inlineCAFs grin
    = let toInline = map funcDefName $ filter (\def -> funcSharing def == NoSharing) (grinFunctions grin)
          cafsToInline = Map.fromList [ (cafName caf, tag) | caf@CAF{cafValue=Node tag FunctionNode 0 []} <- grinCAFs grin, tag `elem` toInline ]
      in runTrans (runReaderT (inlineCAFs') cafsToInline) grin

type M = ReaderT (Map.Map Renamed Renamed) Transform

inlineCAFs' :: M ()
inlineCAFs' = transformExp inlineCAF

inlineCAF :: Expression -> M Expression
inlineCAF (Application fn args)
    = inlineArgs args (Application fn)
inlineCAF (Unit v)
    = inlineValue Unit v
inlineCAF (Store size v)
    = inlineValue (Store size) v
inlineCAF e = tmapM inlineCAF e

inlineValue fn (Variable v)
    = inlineArgs [v] $ \[v'] -> fn (Variable v')
inlineValue fn (Node tag nt missing args)
    = inlineArgs args $ \args' -> fn (Node tag nt missing args')
inlineValue fn (Vector args)
    = inlineArgs args $ \args' -> fn (Vector args')
inlineValue fn value
    = return $ fn value

inlineArgs args fn
    = do m <- ask
         let worker acc []     = return (fn (reverse acc))
             worker acc (x:xs) = case Map.lookup x m of
                                   Nothing  -> worker (x:acc) xs
                                   Just caf -> do v <- newVariable
                                                  rest <- worker (v:acc) xs
                                                  return $ Store 0 (Node caf FunctionNode 0 []) :>>= v :-> rest 
         worker [] args






---------------------------------
-- Inline one-shot functions.

analyzeOneShot :: Grin -> Map.Map Renamed Int
analyzeOneShot grin = Map.unionsWith (+) (map analyzeFuncDef (grinFunctions grin))
    where analyzeFuncDef def = analyzeExpression (funcDefBody def)
          analyzeExpression exp =
            case exp of
              e :>>= v :-> r       -> Map.unionWith (+) (analyzeExpression e) (analyzeExpression r)
              a :>> b              -> Map.unionWith (+) (analyzeExpression a) (analyzeExpression b)
              Application fn args  -> Map.fromList [ (tag, 1) | tag <- fn:args ]
              Case scrut alts      -> Map.insert scrut 1 $
                                      Map.unionsWith (+) [ analyzeExpression branch | _ :> branch <- alts]
              Store _size value    -> analyzeValue value
              Update{}             -> Map.empty
              Unit value           -> analyzeValue value
          analyzeValue (Node tag _ _ _) = Map.singleton tag 1
          analyzeValue _                = Map.empty


inlineOneShot :: Grin -> Grin
inlineOneShot grin
    = runTrans (runReaderT (transformExp inlineOneShotExp) funcMap) grin
    where analysis = analyzeOneShot grin
          funcMap = Map.fromList [ (funcDefName def, def) | def <- grinFunctions grin
                                 , Just 1 <- [Map.lookup (funcDefName def) analysis] ]

type OneShot = ReaderT (Map.Map Renamed FuncDef) Transform

inlineOneShotExp :: Expression -> OneShot Expression
inlineOneShotExp e@(Application fn args)
    = do mbEntry <- asks $ Map.lookup fn
         case mbEntry of
           Nothing   -> return e
           Just func -> doInline func args
inlineOneShotExp e
    = tmapM inlineOneShotExp e

---------------------------------
-- Inline cheap functions.


inlineSimple :: Grin -> Grin
inlineSimple grin
    = let inp = Map.fromList [ (funcDefName def, (funcCategory def, def)) | def <- grinFunctions grin ]
      in runTrans (runReaderT (transformExp inlineSimpleExp) inp) grin

type Simple = ReaderT (Map.Map Renamed (Category, FuncDef)) Transform

inlineSimpleExp :: Expression -> Simple Expression
inlineSimpleExp e@(Store _size (Node tag FunctionNode 0 args))
    = do mbEntry <- findFunc tag
         case mbEntry of
           Just (Cheap, func)
             -> lazify =<< doInline func args
           Just (Lazy, func) | funcSize func < threshold
             -> lazify =<< doInline func args
           _ -> return e
inlineSimpleExp e@(Application fn args)
    = do mbEntry <- findFunc fn
         case mbEntry of
           Just (Cheap, func) -> doInline func args
           Just (Lazy, func)   | funcSize func < threshold
             -> doInline func args
           Just (Strict, func) | funcSize func < threshold
             -> doInline func args
           _ -> return e
inlineSimpleExp e = tmapM inlineSimpleExp e

doInline func args
    = do let renamedArgs = Map.fromList (zip (funcDefArgs func) (args ++ repeat (Builtin "undefined")))
         lift (renameExp renamedArgs (funcDefBody func))

lazify :: Expression -> Simple Expression
lazify (e1 :>>= bind :-> e2)
    = do e2' <- lazify e2
         return $ e1 :>>= bind :-> e2'
lazify (e1 :>> e2)
    = do e2' <- lazify e2
         return $ e1 :>> e2
lazify (Application fn args) | not (isBuiltin fn) && not (isExternal fn)
    = return $ Store 0 (Node fn FunctionNode 0 args)
lazify (Unit v)
    = return $ Store 0 v
lazify (Application (Builtin "eval") [arg])
    = return $ Unit (Variable arg)
lazify e
    = do v <- lift newVariable
         return $ e :>>= v :-> Store 0 (Variable v)

findFunc :: Renamed -> Simple (Maybe (Category, FuncDef))
findFunc name
    = asks $ Map.lookup name







---------------------------------
-- Other stuff


threshold = 5

funcSize :: FuncDef -> Int
funcSize def = expressionSize (funcDefBody def)

expressionSize :: Expression -> Int
expressionSize (e1 :>>= bind :-> e2)
    = expressionSize e1 + expressionSize e2
expressionSize (e1 :>> e2)
    = expressionSize e1 + expressionSize e2
expressionSize (Application fn args)
    = 1
expressionSize (Case scrut alts)
    = sum [ expressionSize branch | _ :> branch <- alts ]
expressionSize Store{}
    = 1
expressionSize Update{}
    = 1
expressionSize Unit{}
    = 1

data Category = NoInline | Lazy | Strict | Cheap deriving (Show,Eq)

instance Monoid Category where
    mempty = Cheap
    mappend NoInline _ = NoInline
    mappend _ NoInline = NoInline
    mappend Strict _ = Strict
    mappend _ Strict = Strict
    mappend Lazy _ = Lazy
    mappend _ Lazy = Lazy
    mappend Cheap Cheap = Cheap

bump :: Category -> Category
bump Cheap = Cheap
bump Lazy = Strict
bump Strict = Strict
bump NoInline = NoInline

funcCategory :: FuncDef -> Category
--funcCategory FuncDef{funcDefBody = Application (Builtin "eval") _}
--    = InlineLazy
funcCategory def = expressionCategory (funcDefBody def)

expressionCategory :: Expression -> Category
expressionCategory (e1 :>>= bind :-> e2)
    = (expressionCategory e1) `mappend` expressionCategory e2
expressionCategory (e1 :>> e2)
    = (expressionCategory e1) `mappend` expressionCategory e2
expressionCategory (Application fn args) | isExternal fn
    = NoInline
expressionCategory (Application (Builtin "eval") _args)
    = Strict
expressionCategory (Application (Builtin "apply") _args)
    = Strict
expressionCategory (Application fn args) | isBuiltin fn
    = Cheap
expressionCategory (Application fn args)
    = Strict
expressionCategory (Case scrut [_ :> branch])
    = expressionCategory branch
expressionCategory (Case scrut alts)
    = NoInline
expressionCategory (Store _size (Node _tag ConstructorNode _n _args))
    = Lazy
expressionCategory (Store _size (Node _tag FunctionNode n _args)) | n >= 1
    = Lazy
expressionCategory Store{}
    = Lazy
expressionCategory Update{}
    = Strict
expressionCategory Unit{}
    = Cheap




data Sharing = NoSharing | Sharing deriving (Eq)

instance Monoid Sharing where
    mempty = NoSharing
    mappend NoSharing NoSharing = NoSharing
    mappend _ _ = Sharing

funcSharing :: FuncDef -> Sharing
funcSharing = expressionSharing . funcDefBody

expressionSharing :: Expression -> Sharing
expressionSharing exp
    = case exp of
        a :>>= l :-> b      -> expressionSharing a `mappend` expressionSharing b
        a :>> b             -> expressionSharing a `mappend` expressionSharing b
        Application fn args -> Sharing
        Case scrut alts     -> Sharing
        Store _size value   -> valueSharing value
        Update{}            -> Sharing
        Unit value          -> NoSharing -- valueSharing value

valueSharing :: Value -> Sharing
valueSharing value
    = case value of
        Node _tag FunctionNode 0 _args -> Sharing
        Node _tag _nt _missing []   -> NoSharing
        Node _tag _nt missingArity _args | missingArity > 0  -> NoSharing
        Node{}                    -> Sharing
        Vector []                 -> NoSharing
        Vector{}                  -> Sharing
        Lit{}                     -> NoSharing
        Variable{}                -> NoSharing
        Hole{}                    -> NoSharing
        Empty                     -> NoSharing





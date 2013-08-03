{-# LANGUAGE OverloadedStrings, PatternGuards #-}
{- |
  The External Core output from GHC suffers from many GHCisms and the direct
  translation to GRIN results in incorrect code.
  This module clears out GHC specific patterns and returns GRIN code fit for use.
  CAUTION: This pass MUST be run exactly once from Grin.FromCore.
           Any deviation will mess things up.
-}
module Grin.Lowering.GHCism
    ( lower
    ) where

import CompactString
import Grin.Types

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map

lower :: Map.Map CompactString Renamed -> Grin -> Grin
lower scope grin
    = case runState (runReaderT (lowerGrin grin) emptyScope) (grinUnique grin) of
        (newGrin, unique) -> newGrin{grinUnique = unique}
    where emptyScope = Scope { scope = scope }

data Scope = Scope { scope :: Map.Map CompactString Renamed }
type Lower a = ReaderT Scope (State Int) a

lowerGrin :: Grin -> Lower Grin
lowerGrin grin
    = do defs <- mapM lowerFuncDef (grinFunctions grin)
         return grin{grinFunctions = defs}

lowerFuncDef :: FuncDef -> Lower FuncDef
lowerFuncDef def
    = do body <- lowerExpression (funcDefBody def)
         return def{funcDefBody = body}

lowerExpression :: Expression -> Lower Expression
lowerExpression (e :>>= lam)
    = do e' <- lowerExpression e
         lam' <- lowerLambda lam
         return $ e' :>>= lam'
lowerExpression (e :>> f)
    = do e' <- lowerExpression e
         f' <- lowerExpression f
         return $ e' :>> f'
lowerExpression (Application (Builtin fn) [a,b]) | Just renamed <- lookup fn renamedOpts
    = lowerExpression (Application (Builtin renamed) [a,b])
lowerExpression (Application (Builtin fn) [a,b]) | fn `elem` [">=#",">#","==#","/=#","<=#","<#","<##",">##",">=##","<=##","==##"
                                                             ,"eqWord#", "neWord#", "leWord#", "geWord#","ltWord#","gtWord#","gtFloat#", "ltFloat#", "geFloat#"
                                                             ,"leFloat#", "eqFloat#"]
    = do tnode <- lookupNode $ fromString "ghc-prim:GHC.Bool.True"
         fnode <- lookupNode $ fromString "ghc-prim:GHC.Bool.False"
         v <- newVariable
         return $ Application (Builtin fn) [a,b] :>>= v :->
                  Case v [Lit (Lint 0) :> Unit (Node fnode ConstructorNode 0 [])
                         ,Lit (Lint 1) :> Unit (Node tnode ConstructorNode 0 [])]

-- MVars
lowerExpression (Application (Builtin "newMVar#") [realWorld])
    = do v <- newVariable
         return $ Store 0 Empty :>>= v :-> Unit (Vector [realWorld, v])
lowerExpression (Application (Builtin "putMVar#") [ptr, val, realWorld])
    = return $ Application (Builtin "updateMutVar") [ptr, val, realWorld]
lowerExpression (Application (Builtin "takeMVar#") [ptr, realWorld])
    = do v <- newVariable
         return $ Application (Builtin "fetch") [ptr] :>>= v :-> Unit (Vector [realWorld, v])

-- MutVars

lowerExpression (Application (Builtin "newMutVar#") [val,realWorld])
    = return $ Application (Builtin "newMutVar") [val, realWorld]
lowerExpression (Application (Builtin "writeMutVar#") [ptr, val, realWorld])
    = return $ Application (Builtin "updateMutVar") [ptr, val, realWorld]
lowerExpression (Application (Builtin "readMutVar#") [ptr, realWorld])
    = return $ Application (Builtin "readMutVar") [ptr, realWorld]
--    = do v <- newVariable
--           return $ Application (Builtin "fetch") [ptr] :>>= v :-> Unit (Vector [realWorld, v])

--lowerExpression (Application (Builtin "realWorld#") [])
--    = return $ Unit Empty -- FIXME: Use a special RealWorld value?
lowerExpression (Application (Builtin "int2Word#") [v])
    = return $ Unit (Variable v)
lowerExpression (Application (Builtin "word2Int#") [v])
    = return $ Unit (Variable v)
lowerExpression (Application (Builtin "plusAddr#") [a,b])
    = return $ Application (Builtin "+#") [a,b]
lowerExpression (Application (Builtin "eqAddr#") [a,b])
    = lowerExpression $ Application (Builtin "==#") [a,b]
--lowerExpression (Application (Builtin fn) [a]) | fn `elem` ["chr#", "ord#"]
--    = return $ Unit (Variable a)

lowerExpression (Application (Builtin "raiseIO#") [exp, realWorld])
    = return $ Application (Builtin "raise#") [exp]

lowerExpression (Application (Builtin "catch#") [fn, handler, realworld])
    = do v <- newVariable
         return $ Application (Builtin "eval") [fn] :>>= v :-> Application (Builtin "apply") [v, realworld]
lowerExpression (Application (Builtin "blockAsyncExceptions#") [fn, realworld])
    = do v <- newVariable
         return $ Application (Builtin "eval") [fn] :>>= v :-> Application (Builtin "apply") [v, realworld]
lowerExpression (Application (Builtin "unblockAsyncExceptions#") [fn, realworld])
    = do v <- newVariable
         return $ Application (Builtin "eval") [fn] :>>= v :-> Application (Builtin "apply") [v, realworld]

lowerExpression (Application (External "lhc_prim_castDoubleToWord" tys) [double, realWorld])
    = do v <- newVariable
         return $ Application (Builtin "coerceDoubleToWord") [double] :>>= v :-> Unit (Vector [realWorld, v])
lowerExpression (Application (External "lhc_prim_castWordToDouble" tys) [word, realWorld])
    = do v <- newVariable
         return $ Application (Builtin "coerceWordToDouble") [word] :>>= v :-> Unit (Vector [realWorld, v])

lowerExpression (Application fn vs)
    = return $ Application fn vs
lowerExpression (Case scrut alts)
    = do alts' <- mapM lowerAlt alts
         return $ Case scrut alts'
lowerExpression (Store size v)
    = return $ Store size v
lowerExpression (Unit v)
    = return $ Unit v
lowerExpression (Update size ptr val)
    = return $ Update size ptr val

lowerLambda :: Lambda -> Lower Lambda
lowerLambda (v :-> e)
    = do e' <- lowerExpression e
         return $ v :-> e'
lowerAlt :: Alt -> Lower Alt
lowerAlt (v :> e)
    = do e' <- lowerExpression e
         return $ v :> e'


renamedOpts = [ ("gtChar#", ">#")
              , ("geChar#", ">=#")
              , ("ltChar#", "<#")
              , ("leChar#", "<=#")
              , ("eqChar#", "==#")
              ]



lookupNode :: CompactString -> Lower Renamed
lookupNode name
    = do m <- asks scope
         case Map.lookup name m of
           Just name -> return name
           Nothing   -> error $ "Couldn't find node: " ++ show name

newVariable :: Lower Renamed
newVariable
    = do u <- get
         put (u+1)
         return $ Anonymous u


{-# LANGUAGE OverloadedStrings #-}
module Grin.Lowering.Apply
    ( lower
    ) where

import CompactString
import Grin.Types

import Control.Monad.Writer
import Control.Monad.State


lower :: Grin -> Grin
lower grin
    = case runState (execWriterT (lowerGrin grin)) (grinUnique grin) of
        ((newDefs),unique) -> grin { grinUnique    = unique
                                   , grinFunctions = appEndo newDefs [] }



type Lower a = WriterT (Endo [FuncDef]) (State Int) a

lowerGrin :: Grin -> Lower ()
lowerGrin grin
    = mapM_ lowerFuncDef (grinFunctions grin)

lowerFuncDef :: FuncDef -> Lower ()
lowerFuncDef def
    = do body <- lowerExpression (funcDefBody def)
         tell $ Endo $ \lst -> def{funcDefBody = body}:lst

lowerExpression :: Expression -> Lower Expression
lowerExpression (e :>>= v :-> t)
    = do e' <- lowerExpression e
         t' <- lowerExpression t
         return (e' :>>= v :-> t')
lowerExpression (e :>> t)
    = do e' <- lowerExpression e
         t' <- lowerExpression t
         return (e' :>> t')
lowerExpression (Application fn args)
    = do return $ Application fn args
lowerExpression (Case v alts)
    = do alts' <- mapM lowerAlt alts
         return $ Case v alts'
lowerExpression (Store size v)
    = liftM (Store size) (lowerValue v)
lowerExpression e@Update{}
    = return e
lowerExpression (Unit v)
    = liftM Unit (lowerValue v)

lowerAlt :: Alt -> Lower Alt
lowerAlt (v :> e)
    = do e' <- lowerExpression e
         return (v :> e')


lowerValue :: Value -> Lower Value
lowerValue (Node (Builtin "evalApply") FunctionNode 0 [a,b])
    = do name <- newApplyFunction
         return $ Node name FunctionNode 0 [a,b]
lowerValue (Node (Builtin "evalApply") _ _ _)
    = error "Grin.Lowering.Apply.lowerValue: Weird apply."
lowerValue (Node name con missing args)
    = do return $ Node name con missing args
lowerValue (Lit lit)    = return $ Lit lit
lowerValue (Vector vs)  = return $ Vector vs
lowerValue (Variable v) = return $ Variable v
lowerValue (Hole size)  = return $ Hole size
lowerValue (Empty)      = return Empty


newApplyFunction
    = do name <- newVariable "apply"
         fnArg <- newVariable "fn"
         valArg <- newVariable "val"
         fn'Arg <- newVariable "fn'"
         tell $ Endo $ \lst -> FuncDef { funcDefName = name
                                       , funcDefArgs = [fnArg, valArg]
                                       , funcDefBody = Application (Builtin "eval") [fnArg] :>>= fn'Arg :-> Application (Builtin "apply") [fn'Arg, valArg]
                                       } : lst
         return name

newVariable :: CompactString -> Lower Renamed
newVariable alias
    = do u <- get
         put (u+1)
         return $ Aliased u alias



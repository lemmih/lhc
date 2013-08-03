{-# LANGUAGE OverloadedStrings #-}
module Grin.Stage3.FromStage2 where

import qualified Data.Map as Map

import qualified Grin.Stage2.Types as Stage2
import qualified Grin.Stage2.Pretty as Stage2
import qualified Grin.Stage2.Transform as Stage2
import Traverse

import Grin.Stage3.Types

import Control.Monad.RWS

convert :: Stage2.Grin -> Grin
convert grin2'
  = Grin { grinNodes = Stage2.grinNodes grin2
         , grinCAFs  = Stage2.grinCAFs grin2
         , grinEntryPoint = Stage2.grinEntryPoint grin2
         , grinUnique = Stage2.grinUnique grin2
         , grinFunctions = map convertFuncDef (Stage2.grinFunctions grin2)
         }
  where grin2 = Stage2.execTrans toNormalForm grin2'

toNormalForm = Stage2.transformExp' worker
  where worker funcdef (Stage2.Case scrut alts)
          = do alts' <- forM alts $ \(cond Stage2.:> exp) ->
                        do exp' <- Stage2.hoistToTopLevel funcdef =<< worker funcdef exp
                           return (cond Stage2.:> exp')
               return $ Stage2.Case scrut alts'
        worker funcdef (Stage2.Case scrut alts Stage2.:>>= binds Stage2.:-> exp)
          = do exp' <- Stage2.hoistToTopLevel funcdef =<< worker funcdef exp
               alts' <- forM alts $ \(cond Stage2.:> altExp) ->
                 do newNames <- mapM Stage2.newVariableFrom binds
                    exp'' <- Stage2.renameExp (Map.fromList (zip binds newNames)) exp'
                    return $ cond Stage2.:> (altExp Stage2.:>>= newNames Stage2.:-> exp'')
               worker funcdef (Stage2.Case scrut alts')
        worker funcdef other = tmapM (worker funcdef) other

convertFuncDef :: Stage2.FuncDef -> FuncDef
convertFuncDef funcDef
  = FuncDef { funcDefName = Stage2.funcDefName funcDef
            , funcDefReturns = Stage2.funcDefReturns funcDef
            , funcDefArgs = Stage2.funcDefArgs funcDef
            , funcDefBody = convertExpression (Stage2.funcDefBody funcDef)
            }
{-
data Expression
    = Singleton SimpleExpression
    | SimpleExpression :>>= Lambda
    | Case        { expValue    :: Renamed
                  , expAlts     :: [Alt] }
    deriving (Eq,Ord)

data SimpleExpression
    = Application { expFunction :: Renamed
                  , expArgs     :: [Renamed] }
    | Fetch       Int Renamed
    | Store       Int [Renamed]
    | StoreHole   Int
    | Unit        [Renamed]
    | Constant    Value
    deriving (Eq,Ord,Show)
data Expression
    = Expression :>>= Lambda
    | Application { expFunction :: Renamed
                  , expArgs     :: [Renamed] }
    | Case        { expValue    :: Renamed
                  , expAlts     :: [Alt] }
    | Fetch       Int Renamed
    | Store       Int [Renamed]
    | StoreHole   Int
    | Unit        [Renamed]
    | Constant    Value
    deriving (Eq,Ord)

-}
convertExpression :: Stage2.Expression -> Expression
convertExpression exp
  = case exp of
      (a Stage2.:>>= binds Stage2.:-> b) Stage2.:>>= (binds' Stage2.:-> c)
        -> convertExpression (a Stage2.:>>= binds Stage2.:-> (b Stage2.:>>= binds' Stage2.:-> c))
      a Stage2.:>>= (binds Stage2.:-> b)
        -> toSimple a :>>= binds :-> convertExpression b
      Stage2.Case scrut alts
        -> Case scrut [ cond :> singleton
                      | cond Stage2.:> exp <- alts 
                      , let singleton = case convertExpression exp of Singleton s -> s; other -> error (show $ Stage2.ppExpression exp) ]
      other
        -> Singleton $ toSimple other
  where toSimple exp = case exp of
          Stage2.Fetch n ptr          -> Fetch n ptr
          Stage2.Application fn args  -> Application fn args
          Stage2.Store n vals         -> Store n vals
          Stage2.StoreHole n          -> StoreHole n
          Stage2.Unit vals            -> Unit vals
          Stage2.Constant val         -> Constant val


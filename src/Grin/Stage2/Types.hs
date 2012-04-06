{-# LANGUAGE TemplateHaskell #-}
module Grin.Stage2.Types
    ( module Grin.Stage2.Types
    , module Grin.Types
    , module Grin.SimpleCore.Types
    ) where

import qualified Grin.Types as Stage1
import Grin.Types (Renamed(..),NodeType(..),NodeDef(..),Type(..),uniqueId, alias
                  ,isBuiltin,isExternal,FFIType(..))

import CompactString
import Traverse

import Grin.SimpleCore.Types (Lit(..))

--import Data.Binary
--import Data.DeriveTH
--import Control.Monad  (ap)

data Grin
    = Grin { grinNodes     :: [NodeDef]
           , grinCAFs      :: [CAF]
           , grinFunctions :: [FuncDef]
           , grinEntryPoint :: Renamed
           , grinUnique    :: Int
           }
    deriving (Eq,Ord)

data CAF
    = CAF { cafName  :: Renamed
          , cafValue :: Value
          }
    deriving (Eq,Ord)

data FuncDef
    = FuncDef { funcDefName :: Renamed
              , funcDefReturns :: Int
              , funcDefArgs :: [Renamed]
              , funcDefBody :: Expression
              }
    deriving (Eq,Ord)

data Lambda = [Renamed] :-> Expression
    deriving (Eq,Ord)
data Alt = Value :> Expression
    deriving (Eq,Ord)

infixr 1 :->
infixr 1 :>>=
infixr 1 :>

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

instance Traverse Expression where
    tmapM fn exp
        = case exp of
            e1 :>>= binds :-> e2
              -> do e1' <- fn e1
                    e2' <- fn e2
                    return (e1' :>>= binds :-> e2')
            Application{}
              -> return exp
            Case scrut alts
              -> do alts' <- sequence [ do alt' <- fn alt
                                           return (cond :> alt')
                                        | cond :> alt <- alts]
                    return $ Case scrut alts'
            Fetch{}
              -> return exp
            Store{}
              -> return exp
            StoreHole{}
              -> return exp
            Unit{}
              -> return exp
            Constant{}
              -> return exp

type Variable = CompactString

data Value
    = Node Renamed NodeType Int
    | Lit Lit
    | Hole
    | Empty
    deriving (Show,Eq,Ord)

{-
$(derive makeBinary ''Value)
$(derive makeBinary ''CAF)
$(derive makeBinary ''FuncDef)
$(derive makeBinary ''Expression)
$(derive makeBinary ''Alt)
$(derive makeBinary ''Lambda)
$(derive makeBinary ''Grin)
-}


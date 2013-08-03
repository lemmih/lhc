{-# LANGUAGE TemplateHaskell #-}
module Grin.Stage3.Types
    ( module Grin.Stage3.Types
    , module Grin.Types
    , module Grin.Stage2.Types
    , module Grin.SimpleCore.Types
    ) where

import qualified Grin.Types as Stage1
import Grin.Types (Renamed(..),NodeType(..),NodeDef(..),Type(..),uniqueId, alias
                  ,isBuiltin,isExternal,FFIType(..))
import Grin.Stage2.Types ( Value(..), CAF(..) )

import CompactString
import Traverse

import Grin.SimpleCore.Types (Lit(..))

data Grin
    = Grin { grinNodes     :: [NodeDef]
           , grinCAFs      :: [CAF]
           , grinFunctions :: [FuncDef]
           , grinEntryPoint :: Renamed
           , grinUnique    :: Int
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
data Alt = Value :> SimpleExpression -- Branch
    deriving (Eq,Ord)
data Branch = Branch Renamed [Renamed]
    deriving (Eq,Ord)

infixr 1 :->
infixr 1 :>>=
infixr 1 :>

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

instance Traverse Expression where
    tmapM fn exp
        = case exp of
            e1 :>>= binds :-> e2
              -> do e2' <- fn e2
                    return (e1 :>>= binds :-> e2')
            Case scrut alts
              -> return $ Case scrut alts
            Singleton{}
              -> return exp

type Variable = CompactString

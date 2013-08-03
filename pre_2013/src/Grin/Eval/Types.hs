{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module Grin.Eval.Types where

import Grin.Types hiding (Value(..))
import qualified Grin.Types as Grin

import qualified Data.Map as Map
import Control.Monad.Reader

import Grin.Types hiding (Value(..))

import qualified Data.Map as Map
import Control.Monad.State
import Data.Char; import Data.Int



type HeapPointer = Int
type Heap = Map.Map HeapPointer EvalValue
data EvalValue
    = CNode Renamed Int [EvalValue]
    | FNode Renamed CompFunction Int [EvalValue]
    | Lit Lit
    | HeapPointer HeapPointer
    | Hole Int
    | Array [EvalValue]
    | Vector [EvalValue]
    | Empty
      deriving (Show,Eq,Ord)

type LocalScope = Map.Map Renamed EvalValue
data GlobalScope = GlobalScope { globalCAFs  :: Map.Map Renamed HeapPointer
                               , globalFuncs :: Map.Map Renamed CompFunction }

type CompFunction = [EvalValue] -> CompExpression
instance Eq CompFunction where _ == _ = False
instance Ord CompFunction where _ `compare` _ = LT
instance Show CompFunction where show _ = "function"
data EvalState
    = EvalState { stateHeap      :: Heap
                , stateFree      :: HeapPointer
                , stateArgs      :: [String] } deriving Show
newtype Comp a = CompExpression { unComp :: StateT EvalState (ReaderT LocalScope IO) a }
    deriving (MonadState EvalState, MonadReader LocalScope, MonadIO, Monad)

type CompExpression = Comp EvalValue
type CompValue = Comp EvalValue
type Gen a = GlobalScope -> a


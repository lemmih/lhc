{-# LANGUAGE TemplateHaskell #-}
module Grin.MinusMinus.Types
    ( module Grin.MinusMinus.Types
    , module Grin.Types
    , module Grin.Stage2.Types
    , module Grin.SimpleCore.Types
    ) where

import CompactString
import Grin.Types (Renamed(..),NodeType(..),NodeDef(..),Type(..),uniqueId, alias
                  ,isBuiltin,isExternal,FFIType(..))
import Grin.Stage2.Types ( Value(..), CAF(..) )

import Traverse

import Grin.SimpleCore.Types (Lit(..))

type Register = Renamed

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
              , funcDefReturnRegisters :: [Register]
              , funcDefArgRegisters :: [Register]
              , funcDefBody :: Body
              }
    deriving (Eq,Ord)

data Alt = Value :> Body
    deriving (Eq,Ord)

infixr 1 :>>
infixr 1 :>

data Body
    = Return
    | TailCall Renamed
    | Statement :>> Body
    | Case        { expValue    :: Renamed
                  , expAlts     :: [Alt] }
    deriving (Eq,Ord)

data Statement
    = Call        { expFunction :: Renamed }
    | CCall       Register String [FFIType] [Register]
    | Fetch       Register Int Renamed
    | Store       Register Int [Register]
    | StoreHole   Register Int -- dest size
    | Move        Register Register -- dest src
    | Constant    Register Value
    | Push        Register
    | Pop         Register
    | Primop      CompactString [Register]
    | PrimopSet   Register CompactString [Register] -- dst op inRegs
    deriving (Eq,Ord)

instance Traverse Body where
    tmapM fn exp
        = case exp of
            e1 :>> e2
              -> do e2' <- fn e2
                    return (e1 :>> e2')
            Case scrut alts
              -> return $ Case scrut alts
            Return{}
              -> return exp
            TailCall{}
              -> return exp


module Language.Haskell.TypeCheck.Types where

import Data.IORef
import Language.Haskell.Exts.SrcLoc

import Language.Haskell.Scope ( Scoped, GlobalName )

-- Type variables are uniquely identified by their name and binding point.
-- The binding point is not enough since ty vars can be bound at an implicit
-- forall.
type TcVar = (String, SrcLoc)
data TcMetaVar = TcMetaRef String (IORef (Maybe TcType))
instance Show TcMetaVar where
    show (TcMetaRef name _) = name
instance Eq TcMetaVar where
    TcMetaRef _ r1 == TcMetaRef _ r2 = r1==r2

data TcType
    = TcForall
    | TcFun TcType TcType
    | TcApp TcType TcType
    -- Uninstantiated tyvar
    | TcVar TcVar
    | TcCon GlobalName
    -- Instantiated tyvar
    | TcMetaVar TcMetaVar
    deriving ( Show )

data Qual t = [Pred] :=> t
    deriving ( Show )
data Pred = IsIn GlobalName TcType
    deriving ( Show )

-- Uninstantiated type signature.
-- eg: forall a. Maybe a -- type of Nothing
-- eg: Int -- type of 10
data Scheme = Scheme [TcVar] (Qual TcType)
    deriving ( Show )

data Typed = Typed TcType Scoped



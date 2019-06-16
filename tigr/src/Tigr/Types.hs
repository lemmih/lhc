module Tigr.Types where

import Data.IORef
import Data.Map (Map)
import Data.Typeable
import qualified Data.Map as Map
import           Control.Exception    (Exception (..), SomeException (..),
                                       handle, throwIO)

data CodeException = CodeException (IORef Thunk)
  deriving (Typeable)
instance Show CodeException where
  show _ = "CodeException"
instance Exception CodeException where
  toException = SomeException

type Name = String

type Context = Map Name (IORef Thunk)
data Closure = Closure Context GCode

data Thunk
  = ThunkCon Name [IORef Thunk]
  | ThunkLit Literal
  | ThunkClosure Closure

data Literal
  = LiteralI64 Int
  | LiteralString String
  deriving (Show, Eq)

{- Common patterns:
Var topLevelFn args
Let var [] (Lit lit) $ ...
LetStrict "" (Var builtin args) $ ...
LetStrict bind (Lit lit) $ ...
LetStrict bind (Con con args) $ ...
Case scrut_whnf
Case with all literals
Case with all constructors
Var CAF -> Var (IORef Thunk)
Var GlobalFn -> VarPartial GCode [Name] | VarComplete GCode [Name] | VarSuper GCode [Name]

GCode = Array of lets + endpoint (var,con,lit,case,lam)
-}
data GCode
  = Var Name [Name]
  | Con Name [Name]
  | External String [Name]
  | Let Name [Name] GCode GCode
  | LetRec [(Name, [Name], GCode)] GCode
  | LetStrict Name GCode GCode
  | Lit Literal
  | Case Name (Maybe GCode) [Alt]
  | Lam [Name] [Name] GCode
  | Throw Name
  | Catch [Name] Name GCode GCode
    deriving (Show)

data Alt = Alt Pattern GCode
  deriving (Show)

data Pattern
  = ConPattern Name [Name]
  | LitPattern Literal
    deriving (Show)

module Compiler.Core where

import           Data.Bedrock           (AvailableNamespace, CType, Foreign,
                                         Name, NodeDefinition, Variable)

data Module = Module
    { coreForeigns  :: [Foreign]
    , coreDecls     :: [Decl]
    , coreNodes     :: [NodeDefinition]
    , coreNamespace :: AvailableNamespace
    }

-- FIXME: Find a better name for this.
data Decl = Decl Name Expr

data Expr
    = Var Variable
    | Con Name [Variable]
    | Lit Literal
    | WithExternal Variable String CType [Variable] Variable Expr
    | ExternalPure String CType [Variable]
    | App Expr Expr
    | Lam [Variable] Expr
    | Let LetBind Expr
    | Case Expr [Alt]
    deriving ( Show )

data LetBind
    = NonRec Variable Expr
    | Rec [(Variable, Expr)]
    deriving ( Show )

data Alt = Alt Pattern Expr
    deriving ( Show )

data Pattern
    = ConPat Name [Variable]
    | LitPat Literal
    deriving ( Show )

-- All unlifted.
data Literal
    = LitChar Char
    | LitString String
    | LitInt Integer
    | LitWord Integer
    | LitFloat Rational
    | LitDouble Rational
    deriving ( Show )


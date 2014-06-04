module Compiler.Core where

import           Data.Bedrock           (AvailableNamespace, CType, Foreign,
                                         Name, NodeDefinition, Variable)
import Data.Bedrock.PrettyPrint ()

import Text.PrettyPrint.ANSI.Leijen

data Module = Module
    { coreForeigns  :: [Foreign]
    , coreDecls     :: [Decl]
    , coreNodes     :: [NodeDefinition]
    , coreNamespace :: AvailableNamespace
    }

instance Pretty Module where
    pretty m = vsep (map pretty (coreDecls m))

-- FIXME: Find a better name for this.
data Decl = Decl Name Expr

instance Pretty Decl where
    pretty (Decl name expr) =
        pretty name <+> equals <$$> indent 2 (pretty expr)

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

rarrow :: Doc
rarrow = text "→ "
ppVars :: [Variable] -> Doc
ppVars = hsep . map pretty


instance Pretty Expr where
    pretty expr =
        case expr of
            Var var -> pretty var
            Con name vars -> pretty name <+> ppVars vars
            Lit lit -> pretty lit
            App a b ->
                pretty a <+> pretty b
            Lam vars e ->
                char 'λ' <+> ppVars vars <+> rarrow <$$> indent 2 (pretty e)
            Case scrut alts ->
                text "case" <+> pretty scrut <+> text "of" <$$>
                indent 2 (vsep $ map pretty alts)

data LetBind
    = NonRec Variable Expr
    | Rec [(Variable, Expr)]
    deriving ( Show )

data Alt = Alt Pattern Expr
    deriving ( Show )

instance Pretty Alt where
    pretty (Alt pattern expr) =
        pretty pattern <+> rarrow <$$> indent 2 (pretty expr)

data Pattern
    = ConPat Name [Variable]
    | LitPat Literal
    deriving ( Show )

instance Pretty Pattern where
    pretty pattern =
        case pattern of
            ConPat name vars ->
                pretty name <+> ppVars vars
            LitPat lit -> pretty lit

-- All unlifted.
data Literal
    = LitChar Char
    | LitString String
    | LitInt Integer
    | LitWord Integer
    | LitFloat Rational
    | LitDouble Rational
    deriving ( Show )

instance Pretty Literal where
    pretty lit =
        case lit of
            LitChar c -> pretty c
            LitInt i   -> pretty i


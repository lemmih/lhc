module Compiler.Core where

import           Data.Bedrock           (AvailableNamespace, CType, Foreign,
                                         Name )
import Data.Bedrock.PrettyPrint ()
import Language.Haskell.TypeCheck.Types (Coercion(..), TcType)

import Text.PrettyPrint.ANSI.Leijen

data Module = Module
    { coreForeigns  :: [Foreign]
    , coreDecls     :: [Decl]
    , coreNodes     :: [NodeDefinition]
    , coreNamespace :: AvailableNamespace
    }

instance Pretty Module where
    pretty m = vsep
        [ vsep (map pretty (coreNodes m))
        , vsep (map pretty (coreDecls m))
        ]

-- FIXME: Find a better name for this.
data Decl = Decl TcType Name Expr

instance Pretty Decl where
    pretty (Decl ty name expr) =
        pretty name <+> colon <+> pretty ty <$$>
        pretty name <+> equals <$$> indent 2 (pretty expr)

data NodeDefinition = NodeDefinition Name [TcType]
    deriving (Show)

instance Pretty NodeDefinition where
    pretty (NodeDefinition name args) =
        text "node" <+> pretty name <+> hsep (map pretty args)


data Variable = Variable
    { varName :: Name
    , varType :: TcType
    } deriving ( Show )

data Expr
    = Var Variable
    | Con Name [Variable]
    | Lit Literal
    | WithExternal Variable String [Variable] Variable Expr
    -- | ExternalPure String CType [Variable]
    | App Expr Expr
    | Lam [Variable] Expr
    | Let LetBind Expr
    | Case Expr [Alt]
    | Id
    | WithCoercion Coercion Expr
    deriving ( Show )

rarrow :: Doc
rarrow = text "→ "
ppVars :: [Variable] -> Doc
ppVars = hsep . map pretty

ppTypedVars :: [Variable] -> Doc
ppTypedVars = hsep . map ppTypedVariable

instance Pretty Expr where
    pretty expr =
        case expr of
            Var var -> pretty var
            Con name vars -> pretty name <+> ppVars vars
            Lit lit -> pretty lit
            App a b ->
                pretty a <+> parens (pretty b)
            Lam vars e ->
                char 'λ' <+> ppTypedVars vars <+> rarrow <$$> indent 2 (pretty e)
            Case scrut alts ->
                text "case" <+> pretty scrut <+> text "of" <$$>
                indent 2 (vsep $ map pretty alts)
            Id -> text "id"
            WithCoercion CoerceId e -> pretty e
            WithCoercion c e ->
                parens (pretty c) <+> pretty e
            WithExternal outV cName args st cont ->
                ppTypedVariable outV <+> text "←" <+>
                    text "external" <+> pretty cName <+> ppVars args <$$>
                pretty cont
            Let (NonRec name e1) e2 ->
                text "let" <+> pretty name <+> equals <+> pretty e1 <$$>
                pretty e2

ppTypedVariable :: Variable -> Doc
ppTypedVariable var = parens $
    pretty (varName var) <> colon <> pretty (varType var)

instance Pretty Variable where
    pretty var = pretty (varName var)

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
                pretty name <+> ppTypedVars vars
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
            LitChar c     -> pretty c
            LitInt i      -> pretty i
            LitString str -> pretty str


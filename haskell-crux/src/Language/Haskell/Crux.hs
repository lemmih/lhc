{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.Haskell.Crux where

import           Codec.Serialise
import           Data.List
import qualified Data.Semigroup                    as Semigroup
import           GHC.Generics
import           Language.Haskell.Exts             (SrcSpan, SrcSpanInfo)
import qualified Language.Haskell.Scope            as Scope
import           Language.Haskell.TypeCheck        (Type (..))
import qualified Language.Haskell.TypeCheck        as TC
import           Language.Haskell.TypeCheck.Pretty
import qualified LLVM.AST                          as LLVM (Type)
import qualified LLVM.AST                          as LLVM
import qualified LLVM.AST.AddrSpace                as LLVM (AddrSpace)
import qualified Text.PrettyPrint.ANSI.Leijen      as Doc

data Module = Module
    { cruxForeigns :: [Foreign]
    , cruxDecls    :: [Declaration]
    , cruxNodes    :: [NodeDefinition]
    , cruxNewTypes :: [NewType]
    } deriving (Show, Generic)

data Foreign = Foreign
  { foreignName      :: String
  , foreignReturn    :: LLVM.Type
  , foreignArguments :: [LLVM.Type]
  } deriving (Show, Read, Eq, Generic)

data Name = Name
  { nameModule     :: [String]
  , nameIdentifier :: String
  , nameUnique     :: Int
  } deriving (Show, Read, Eq, Ord, Generic)

data Declaration = Declaration
  { declType :: Type
  , declName :: Name
  , declBody :: Expr }
  deriving (Show, Generic)

data NodeDefinition = NodeDefinition Variable
    deriving (Show, Generic)

newtype NewType = IsNewType Variable deriving (Show, Eq, Generic)

data Variable = Variable
    { varName :: Name
    , varType :: Type
    } deriving ( Show, Eq, Ord, Generic )

data Expr
    = Var Variable
    | Con Variable
    | UnboxedTuple [Expr]
    | Lit Literal
    | WithExternal Variable Variable String [Expr] Expr Expr
    | ExternalPure Variable String [Expr] Expr
    | App Expr Expr
    | Lam [Variable] Expr
    | Let LetBind Expr
    | LetStrict Variable Expr Expr
    | Case Expr Variable (Maybe Expr) [Alt]
    | Convert Expr Type
    | Cast
    deriving ( Show, Eq, Generic )

data LetBind
    = NonRec Variable Expr
    | Rec [(Variable, Expr)]
    deriving ( Show, Eq, Generic )

data Alt = Alt Pattern Expr
    deriving ( Show, Eq, Generic )

data Pattern
    = ConPat Variable [Variable]
    | LitPat Literal
    | UnboxedPat [Variable]
    -- VarPat Variable
    deriving ( Show, Eq, Generic )

-- All unlifted.
data Literal
    = LitChar Char
    | LitString String
    | LitInt Integer
    | LitWord Integer
    | LitFloat Rational
    | LitDouble Rational
    | LitVoid
    deriving ( Show, Eq, Generic )


--------------------------------------------------------------
-- Instances


instance Semigroup.Semigroup Module where
  a <> b = Module
    { cruxForeigns = cruxForeigns a ++ cruxForeigns b
    , cruxDecls = cruxDecls a ++ cruxDecls b
    , cruxNodes = cruxNodes a ++ cruxNodes b
    , cruxNewTypes = cruxNewTypes a ++ cruxNewTypes b }
instance Monoid Module where
    mempty = Module [] [] [] []
    mappend = (Semigroup.<>)

instance Pretty Module where
    pretty m = vsep $ concat
        [ map pretty (cruxNodes m)
        , map pretty (cruxNewTypes m)
        , map pretty (cruxDecls m)
        ]

instance Pretty Name where
  pretty name =
    text (intercalate "." (nameModule name ++ [nameIdentifier name]))
    <> unique
    where
      unique = if nameUnique name == 0
        then Doc.empty
        else char '^' <> int (nameUnique name)

instance Pretty LLVM.Type where
  pretty _ = text "<LLVM.Type>"

instance Pretty Foreign where
  pretty f =
    ppSyntax "foreign" <+> pretty (foreignReturn f) <+>
    text (foreignName f) <>
    Doc.parens (ppList (map pretty $ foreignArguments f))

instance Pretty Declaration where
    pretty (Declaration ty name expr) =
        pretty name <+> colon <+> pretty ty <$$>
        pretty name <+> equals <$$> indent 2 (pretty expr)

instance Pretty NodeDefinition where
  pretty (NodeDefinition node) =
    text "node" <+> ppTypedVariable node
    -- pretty (NodeDefinition name args) =
    --     text "node" <+> pretty name <+> hsep (map (prettyPrec appPrecedence) args)

instance Pretty NewType where
    pretty (IsNewType con) =
        text "newtype" <+> ppTypedVariable con

rarrow :: Doc
rarrow = text "→ "
ppVars :: [Expr] -> Doc
ppVars = hsep . map (prettyPrec appPrecedence)

ppTypedVars :: [Variable] -> Doc
ppTypedVars = hsep . map ppTypedVariable

appPrecedence :: Int
appPrecedence = 2

instance Pretty Expr where
  prettyPrec p expr =
    case expr of
      Var var -> pretty var
      -- Var var -> ppTypedVariable var
      Con name -> pretty name -- <+> ppVars vars
      UnboxedTuple args ->
        text "(#" <+>
        (hsep $ punctuate comma $ map pretty args) <+>
        text "#)"
      Lit lit -> pretty lit
      App a b -> parensIf (p >= appPrecedence) $ nest 2 $
        prettyPrec 1 a </> prettyPrec appPrecedence b
      Lam vars e -> parensIf (p > 0) $
        char 'λ' <+> ppTypedVars vars <+> rarrow <$$> pretty e
      Case scrut var Nothing [Alt pattern e] -> parensIf (p > 0) $
        pretty var <+> text "←" <+> align (pretty scrut) <$$>
        pretty pattern <+> text "←" <+> ppTypedVariable var <$$>
        pretty e
      Case scrut var Nothing alts -> parensIf (p > 0) $
        text "case" <+> hang 2 (pretty scrut) <+> text "of" <$$>
        indent 2 (ppTypedVariable var <$$> vsep (map pretty alts))
      Case scrut var (Just defaultBranch) alts -> parensIf (p > 0) $
        text "case" <+> hang 2 (pretty scrut) <+> text "of" <$$>
        indent 2 ( ppTypedVariable var <$$> vsep (map pretty alts) <$$>
          text "DEFAULT" <+> rarrow <$$> indent 2 (pretty defaultBranch))
      Convert scrut ty ->
        parens (pretty scrut <+> text ":::" <+> pretty ty)
      Cast ->
        text "cast"
      WithExternal outV outS cName args _st cont ->
        ppTypedVariable outV <> comma <+> pretty outS <+> text "←" <+>
          text "external" <+> text cName <+> ppVars args <$$>
        pretty cont
      ExternalPure outV cName args cont ->
        ppTypedVariable outV <+> text "←" <+>
          text "external" <+> text cName <+> ppVars args <$$>
        pretty cont
      Let (NonRec name e1) e2 ->
          text "let" <+> ppTypedVariable name <+> equals <+> align (pretty e1) <$$>
          pretty e2
      Let (Rec binds) e2 ->
          text "let" <$$>
          indent 2 (vsep [ ppTypedVariable var <+>
                           equals <+>
                           hang 0 (pretty body)
                         | (var,body) <- binds ]) <$$>
          text "in" <$$>
          indent 2 (pretty e2)
      LetStrict name e1 e2 ->
          text "let" <+> char '!' <> ppTypedVariable name <+> equals <+> pretty e1 <$$>
          pretty e2

ppTypedVariable :: Variable -> Doc
ppTypedVariable var =
    pretty (varName var) <> colon <> prettyPrec 2 (varType var)

instance Pretty Variable where
  -- pretty = ppTypedVariable
  pretty var = pretty (varName var)

instance Pretty Alt where
    pretty (Alt pattern expr) =
        pretty pattern <+> rarrow <$$> indent 2 (pretty expr)

instance Pretty Pattern where
    pretty pattern =
        case pattern of
            ConPat name vars ->
                pretty name <+> ppTypedVars vars
            LitPat lit -> pretty lit
            UnboxedPat vars ->
                text "(#" <+>
                (hsep $ punctuate comma $ map pretty vars) <+>
                text "#)"
            -- VarPat var ->
            --     ppTypedVariable var

instance Pretty Literal where
    pretty lit =
        case lit of
            LitChar c     -> text $ show c
            LitInt i      -> integer i
            LitString str -> text $ show str
            LitVoid       -> text "void"
            _             -> text "{literal}"

ppSyntax :: String -> Doc
ppSyntax = Doc.green . text

ppList :: [Doc] -> Doc
ppList = Doc.hsep . Doc.punctuate (Doc.char ',')


instance Serialise Module
instance Serialise Foreign
instance Serialise Declaration
instance Serialise Type
instance Serialise Name
instance Serialise Expr
instance Serialise LetBind
instance Serialise Alt
instance Serialise Variable
instance Serialise Literal
instance Serialise Pattern
instance Serialise NodeDefinition
instance Serialise NewType
instance Serialise TC.Predicate
instance Serialise TC.TyVar
instance Serialise Scope.Entity
instance Serialise SrcSpanInfo
instance Serialise SrcSpan
instance Serialise Scope.QualifiedName
instance Serialise Scope.EntityKind
instance Serialise a => Serialise (TC.Qualified a)

instance Serialise LLVM.Type
instance Serialise LLVM.AddrSpace
instance Serialise LLVM.FloatingPointType
instance Serialise LLVM.Name

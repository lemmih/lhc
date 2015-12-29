{-# LANGUAGE TemplateHaskell #-}
module Compiler.Core where

import           Data.Bedrock                      (AvailableNamespace (..),
                                                    CType (..), Foreign,
                                                    Foreign (..), Name (..))
import           Data.Bedrock.PrettyPrint          ()
import           Data.Binary
import           Data.Derive.Binary
import           Data.DeriveTH
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Scope            (GlobalName (..),
                                                    QualifiedName (..))
import           Language.Haskell.TypeCheck.Pretty
import           Language.Haskell.TypeCheck.Types  (Coercion (..), Pred (..),
                                                    Qual (..), TcType (..),
                                                    TcVar (..), TcMetaVar(..))

data Module = Module
    { coreForeigns  :: [Foreign]
    , coreDecls     :: [Decl]
    , coreNodes     :: [NodeDefinition]
    , coreNewTypes  :: [NewType]
    , coreNamespace :: AvailableNamespace
    }

instance Monoid Module where
    mempty = Module [] [] [] [] (AvailableNamespace 0 0 0 0)
    mappend a b = Module
        { coreForeigns = coreForeigns a ++ coreForeigns b
        , coreDecls = coreDecls a ++ coreDecls b
        , coreNodes = coreNodes a ++ coreNodes b
        , coreNewTypes = coreNewTypes a ++ coreNewTypes b
        , coreNamespace = coreNamespace a }

instance Pretty Module where
    pretty m = vsep
        [ vsep (map pretty (coreNodes m))
        , vsep (map pretty (coreNewTypes m))
        , vsep (map pretty (coreDecls m))
        ]

-- FIXME: Find a better name for this.
data Decl = Decl
  { declType :: TcType
  , declName :: Name
  , declBody :: Expr }

instance Pretty Decl where
    pretty (Decl ty name expr) =
        pretty name <+> colon <+> pretty ty <$$>
        pretty name <+> equals <$$> indent 2 (pretty expr)

data NodeDefinition = NodeDefinition Name [TcType]
    deriving (Show)

instance Pretty NodeDefinition where
    pretty (NodeDefinition name args) =
        text "node" <+> pretty name <+> hsep (map pretty args)

data NewType = NewType Name

instance Pretty NewType where
    pretty (NewType con) =
        text "newtype" <+> pretty con

data Variable = Variable
    { varName :: Name
    , varType :: TcType
    } deriving ( Show, Eq, Ord )

data Expr
    = Var Variable
    | Con Name
    | UnboxedTuple [Expr]
    | Lit Literal
    | WithExternal Variable String [Variable] Variable Expr
    | ExternalPure Variable String [Variable] Expr
    | App Expr Expr
    | Lam [Variable] Expr
    | Let LetBind Expr
    | LetStrict Variable Expr Expr
    | Case Expr Variable (Maybe Expr) [Alt]
    | Cast Expr TcType
    | Id
    | WithCoercion Coercion Expr
    deriving ( Show )

rarrow :: Doc
rarrow = text "→ "
ppVars :: [Variable] -> Doc
ppVars = hsep . map pretty

ppTypedVars :: [Variable] -> Doc
ppTypedVars = hsep . map ppTypedVariable

appPrecedence :: Int
appPrecedence = 1

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
      App a b -> parensIf (p > 0) $ nest 2 $
        pretty a </> prettyPrec appPrecedence b
      Lam vars e -> parensIf (p > 0) $
        char 'λ' <+> ppTypedVars vars <+> rarrow <$$> pretty e
      Case scrut var Nothing [Alt pattern e] ->
        pretty var <+> text "←" <+> align (pretty scrut) <$$>
        pretty pattern <+> text "←" <+> ppTypedVariable var <$$>
        pretty e
      Case scrut var Nothing alts ->
        text "case" <+> hang 2 (pretty scrut) <+> text "of" <$$>
        indent 2 (ppTypedVariable var <$$> vsep (map pretty alts))
      Case scrut var (Just defaultBranch) alts ->
        text "case" <+> hang 2 (pretty scrut) <+> text "of" <$$>
        indent 2 ( ppTypedVariable var <$$> vsep (map pretty alts) <$$>
          text "DEFAULT" <+> rarrow <$$> indent 2 (pretty defaultBranch))
      Cast expr ty ->
        parens (pretty expr <+> text ":::" <+> pretty ty)
      Id -> text "id"
      WithCoercion CoerceId e -> pretty e
      WithCoercion (CoerceAp tys) e -> parensIf (p > 0) $
        pretty e <+> fillSep (map (prettyPrec appPrecedence) tys)
      WithCoercion c e -> parensIf (p > 0) $
        pretty c <+> pretty e
      WithExternal outV cName args st cont ->
        ppTypedVariable outV <+> text "←" <+>
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
    | UnboxedPat [Variable]
    -- | VarPat Variable
    deriving ( Show )

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
            LitChar c     -> text $ show c
            LitInt i      -> integer i
            LitString str -> text $ show str

-- FIXME: Move orphan instance to their rightful modules.

instance Binary TcMetaVar where
    put = error "Binary.put not defined for TcMetaRef"
    get = error "Binary.get not defined for TcMetaVar"

derive makeBinary ''SrcSpan
derive makeBinary ''SrcSpanInfo
derive makeBinary ''QualifiedName
derive makeBinary ''GlobalName
derive makeBinary ''Pred
derive makeBinary ''Qual
derive makeBinary ''TcVar
derive makeBinary ''TcType
derive makeBinary ''Name
derive makeBinary ''Variable
derive makeBinary ''Literal
derive makeBinary ''Pattern
derive makeBinary ''LetBind
derive makeBinary ''Coercion
derive makeBinary ''NodeDefinition
derive makeBinary ''CType
derive makeBinary ''Foreign
derive makeBinary ''Alt
derive makeBinary ''Expr
derive makeBinary ''Decl
derive makeBinary ''AvailableNamespace
derive makeBinary ''NewType
derive makeBinary ''Module





{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
module Data.Bedrock.PrettyPrint
  ( Pretty(..)
  , module Data.Bedrock.PrettyPrint
  ) where

import           Data.Bedrock
import           Data.List                             (intercalate)
import Data.Text.Lazy (unpack)
import           Language.Haskell.TypeCheck.Pretty     (Pretty (..))
import           Text.PrettyPrint.ANSI.Leijen          (Doc, brackets, char,
                                                        empty, int, parens,
                                                        text, (<>))
import qualified Text.PrettyPrint.ANSI.Leijen          as Doc
import qualified Text.PrettyPrint.ANSI.Leijen.Internal as Doc
import qualified LLVM.AST                   as LLVM (Type (..))
import LLVM.Pretty

-------------------------------------------------------------------------------
-- Pretty print helpers

(<+>) :: Doc -> Doc -> Doc
Doc.Empty <+> b = b
a <+> b = a Doc.<+> b

(<$$>) :: Doc -> Doc -> Doc
Doc.Empty <$$> b = b
a <$$> b = a Doc.<$$> b

-------------------------------------------------------------------------------
-- Pretty print

instance Pretty Name where
  pretty name =
    text (intercalate "." (nameModule name ++ [nameIdentifier name]))
    <> unique
    where
      unique = if nameUnique name == 0
        then Doc.empty
        else char '^' <> int (nameUnique name)

instance Pretty Type where
  pretty NodePtr        = Doc.char '*'
  pretty Node           = Doc.char '%'
  pretty (StaticNode n) = Doc.char '%' <> Doc.int n <> Doc.char '%'
  pretty IWord          = Doc.char '#'
  pretty (Primitive ty) = pretty ty
  pretty FramePtr       = Doc.red (Doc.char '@')

instance Pretty LLVM.Type where
  pretty ty = text (unpack $ ppll ty)
  -- pretty I8 = text "i8"
  -- pretty I32 = text "i32"
  -- pretty I64 = text "i64"
  -- pretty (CPointer ty) = pretty ty <> Doc.char '*'
  -- pretty (CFunction retTy argTys) =
  --   pretty retTy <> Doc.parens (ppList (map pretty argTys))
  -- pretty CVoid = text "void"

instance Pretty NodeDefinition where
  pretty (NodeDefinition name args) =
    text "node" <+> Doc.blue (pretty name) <> Doc.parens (ppList (map pretty args))

instance Pretty Variable where
  pretty Variable{ variableName = name, variableType = Primitive ty } =
    pretty ty <> char '|' <> pretty name
  pretty Variable{ variableName = name, variableType = ty } =
    pretty ty <> pretty name

instance Pretty Literal where
  pretty literal =
    case literal of
      LiteralInt i      -> Doc.integer i
      LiteralString str -> Doc.text (show str)

ppNode :: NodeName -> [Doc] -> Doc
ppNode (ConstructorName constructor blanks) args =
  Doc.hsep (Doc.blue (pretty constructor) : args ++ replicate blanks (Doc.char '_'))
ppNode (FunctionName fn blanks) args =
  Doc.hsep (Doc.magenta (pretty fn) : args ++ replicate blanks (Doc.char '_'))
ppNode UnboxedTupleName args =
  Doc.text "(#" <+> ppList args <+> Doc.text "#)"
--ppNode (CatchFrame fn blanks) args =
--  Doc.hsep (Doc.green (ppName fn) : args ++ replicate blanks (Doc.char '_'))

-- layout Node _ : 10 10
instance Pretty NodeLayout where
  pretty (NodeLayout name prims ptrs) =
    text "layout" <+> ppNode name [] <+> char ':' <+> int prims <+> int ptrs

ppPattern :: Pattern -> Doc
ppPattern pattern =
  case pattern of
    NodePat name binds -> ppNode name (map pretty binds)
    LitPat lit         -> pretty lit
    -- UnboxedPat binds   -> Doc.text "(#" <+> ppList (map pretty binds) <+> Doc.text "#)"
    -- VarPat var         -> pretty var

ppList :: [Doc] -> Doc
ppList = Doc.hsep . Doc.punctuate (Doc.char ',')

ppAlternative :: Alternative -> Doc
ppAlternative (Alternative pattern expression) =
  ppPattern pattern <+> text "→" Doc.<$$>
  Doc.indent 2 (ppBlock expression)

instance Pretty MemAttributes where
  pretty MemAttributes{ memConstant = constant, memAliasGroup = g} =
    if constant then ppSyntax "constant" else Doc.empty <+>
    case g of
      Nothing    -> Doc.empty
      Just group -> ppSyntax "alias" <> Doc.char ':' <> Doc.int group

instance Pretty Parameter where
  pretty =
    \case
      PInt n -> Doc.int n
      PString str -> Doc.text (show str)
      PName name -> pretty name
      PNodeName node -> Doc.text "node" <+> ppNode node []
      PVariable var -> pretty var
      PVariables var -> Doc.brackets (ppList $ map pretty var)

instance Pretty Expression where
  pretty simple =
    case simple of
      Builtin str params ->
        ppSyntax ('@':str) <> Doc.parens (ppList $ map pretty params)
      Application fn args ->
        pretty fn <> Doc.parens (ppList $ map pretty args)
      CCall fn args ->
        ppSyntax "@ccall" <+>
        text fn <> Doc.parens (ppList $ map pretty args)
      Catch exh exhArgs fn args ->
        ppSyntax "@catch" <+>
        pretty exh <> Doc.parens (ppList $ map pretty exhArgs) <+>
        pretty fn <> Doc.parens (ppList $ map pretty args)
      InvokeReturn cont args ->
        ppSyntax "@invoke" <+> pretty cont <>
        Doc.parens (ppList (map pretty args))
      Literal lit ->
        ppSyntax "@literal" <+> pretty lit

instance Pretty Block where
  pretty = ppBlock

ppBlock :: Block -> Doc
ppBlock block =
  case block of
    Return args ->
      ppSyntax "@return" <>
      parens (ppList (map pretty args))
    Bind [] simple rest ->
      pretty simple Doc.<$$>
      ppBlock rest
    Bind names simple rest ->
      ppList (map pretty names) <+>
      text "=" <+>
      pretty simple Doc.<$$>
      ppBlock rest
    Case scrut Nothing [(Alternative pattern expression)] ->
      ppPattern pattern <+> text "← " <+> pretty scrut Doc.<$$>
      ppBlock expression
    Case scrut Nothing alts ->
      ppSyntax "case" <+> pretty scrut <+> ppSyntax "of" <+> Doc.lbrace Doc.<$$>
      Doc.indent 2 (Doc.vsep $ map ppAlternative alts) Doc.<$$> Doc.rbrace
    Case scrut (Just branch) alts ->
      ppSyntax "case" <+> pretty scrut <+> ppSyntax "of" <+> Doc.lbrace Doc.<$$>
      Doc.indent 2 (Doc.vsep (map ppAlternative alts ++
        [text "DEFAULT" <+> text "→" Doc.<$$>
          Doc.indent 2 (ppBlock branch)])) Doc.<$$> Doc.rbrace
    Raise obj ->
      ppSyntax "@raise" <+> pretty obj
    TailCall fn args ->
      ppSyntax "@tail" <+>
      pretty fn <> Doc.parens (ppList (map pretty args))
    Invoke cont args ->
      ppSyntax "@invoke" <+> pretty cont <>
      Doc.parens (ppList (map pretty args))
    Exit ->
      ppSyntax "@exit"
    Panic msg ->
      ppSyntax "@panic" <+> text (show msg)

ppSyntax :: String -> Doc
ppSyntax = Doc.green . text

ppFnName :: Name -> Doc
ppFnName = Doc.blue . pretty

ppTypes :: [Type] -> Doc
ppTypes = ppList . map pretty

instance Pretty Attribute where
  pretty NoCPS    = text "NoCPS"
  pretty Internal = text "Internal"
  pretty (Prefix size prim ptrs mbHandler) =
    text "prefix"
      <+> int size
      <+> brackets (int prim)
      <+> brackets (int ptrs)
      <+> maybe empty pretty mbHandler

instance Pretty Function where
  pretty fn =
    ppList (map pretty (fnAttributes fn)) Doc.<$$>
    ppTypes (fnResults fn) <+>
    ppFnName (fnName fn) <+> Doc.parens (ppList $ map pretty (fnArguments fn)) Doc.<$$>
    Doc.indent 2 (ppBlock (fnBody fn))

ppEntryPoint :: Name -> Doc
ppEntryPoint entry = text "entrypoint:" <+> pretty entry

instance Pretty Foreign where
  pretty f =
    ppSyntax "foreign" <+> pretty (foreignReturn f) <+>
    text (foreignName f) <>
    Doc.parens (ppList (map pretty $ foreignArguments f))

instance Pretty Module where
  pretty m =
    Doc.vsep (
      map pretty (modForeigns m) ++
      map pretty (nodes m) ++
      map pretty (modLayouts m)) <$$>
    ppEntryPoint (entryPoint m) <$$>
    Doc.vsep (
      map pretty (functions m))

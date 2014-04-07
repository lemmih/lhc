module Data.Bedrock.PrettyPrint where

import           Text.PrettyPrint.ANSI.Leijen (Doc, char, int, text, (<+>),
                                               (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

import           Data.Bedrock

-------------------------------------------------------------------------------
-- Pretty print

ppName :: Name -> Doc
ppName name =
	if nameUnique name == 0
		then text (nameIdentifier name)
		else text (nameIdentifier name) <> char '^' <> int (nameUnique name)

ppType :: Type -> Doc
ppType NodePtr   = text ""
ppType RawNode   = Doc.char ':' <> ppSyntax "node"
ppType Primitive = Doc.char ':' <> ppSyntax "prim"

ppVariable :: Variable -> Doc
ppVariable Variable{ variableName = name, variableType = ty } =
	ppName name <> ppType ty

ppArgument :: Argument -> Doc
ppArgument arg =
	case arg of
		RefArg name       -> ppVariable name
		LitArg lit        -> ppLiteral lit
		NodeArg name args -> Doc.parens (ppNode name (map ppArgument args))

ppLiteral :: Literal -> Doc
ppLiteral literal =
	case literal of
		LiteralInt i -> Doc.integer i

ppNode :: NodeName -> [Doc] -> Doc
ppNode (ConstructorName constructor) args =
	Doc.hsep (ppName constructor : args)
ppNode (FunctionName fn blanks) args =
	Doc.hsep (ppName fn : args ++ replicate blanks (Doc.char '_'))

ppPattern :: Pattern -> Doc
ppPattern pattern =
	case pattern of
		NodePat name binds -> ppNode name (map ppVariable binds)
		LitPat lit         -> ppLiteral lit

ppList :: [Doc] -> Doc
ppList = Doc.hsep . Doc.punctuate (Doc.char ',')

ppAlternative :: Alternative -> Doc
ppAlternative (Alternative pattern expression) =
	ppPattern pattern <+> text "→" Doc.<$$>
	Doc.indent 2 (ppExpression expression)

ppSimpleExpression :: SimpleExpression -> Doc
ppSimpleExpression simple =
	case simple of
		Alloc n ->
			ppSyntax "@alloc" <+> Doc.int n
		Store node args ->
			ppSyntax "@store" <+>
			Doc.parens (ppNode node (map ppArgument args))
		Fetch ptr ->
			ppSyntax "@fetch" <+> ppVariable ptr
		Print var ->
			ppSyntax "@print" <+> ppVariable var
		Add lhs rhs ->
			ppSyntax "@add" <+> ppArgument lhs <+> ppArgument rhs
		Application fn args ->
			ppName fn <> Doc.parens (ppList $ map ppArgument args)
		WithExceptionHandler exh exhArgs fn args ->
			ppSyntax "@withExceptionHandler" <+>
			ppName exh <> Doc.parens (ppList $ map ppArgument exhArgs) <+>
			ppName fn <> Doc.parens (ppList $ map ppArgument args)

ppExpression :: Expression -> Doc
ppExpression expression =
	case expression of
		Return args ->
			ppSyntax "@return" <+>
			ppList (map ppArgument args)
		Bind [] simple rest ->
			ppSimpleExpression simple <> Doc.char ';' Doc.<$$>
			ppExpression rest
		Bind names simple rest ->
			ppList (map ppVariable names) <+>
			text ":=" <+>
			ppSimpleExpression simple <> Doc.char ';' Doc.<$$>
			ppExpression rest
		Case scrut alts _defaultBranch ->
			ppSyntax "case" <+> ppVariable scrut <+> ppSyntax "of" Doc.<$$>
			Doc.indent 2 (Doc.vsep $ map ppAlternative alts)
		Throw obj ->
			ppSyntax "@throw" <+> ppArgument obj
		TailCall fn args ->
			ppSyntax "@tail" <+>
			ppName fn <> Doc.parens (ppList (map ppArgument args))
		Invoke cont args ->
			ppSyntax "@invoke" <>
			Doc.parens (ppList (ppVariable cont : map ppArgument args))
		Exit ->
			ppSyntax "@exit"

ppSyntax :: String -> Doc
ppSyntax = Doc.green . text

ppFnName :: Name -> Doc
ppFnName = Doc.blue . ppName

ppFunction :: Function -> Doc
ppFunction fn =
	ppFnName (fnName fn) <+> Doc.hsep (map ppVariable (fnArguments fn)) <+>
	Doc.char '=' Doc.<$$>
	Doc.indent 2 (ppExpression (fnBody fn))

ppModule :: Module -> Doc
ppModule m = Doc.vsep (map ppFunction (functions m))



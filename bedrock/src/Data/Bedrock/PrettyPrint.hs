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
ppType NodePtr        = Doc.char '*'
ppType Node           = Doc.char '%'
ppType (StaticNode n) = Doc.char '%' <> Doc.int n <> Doc.char '%'
ppType Primitive      = Doc.char '#'

ppNodeDefinition :: NodeDefinition -> Doc
ppNodeDefinition (NodeDefinition name args) =
	text "node" <+> ppNode (ConstructorName name) (map ppType args)

ppVariable :: Variable -> Doc
ppVariable Variable{ variableName = name, variableType = ty } =
	ppType ty <> ppName name

ppArgument :: Argument -> Doc
ppArgument arg =
	case arg of
		RefArg name       -> ppVariable name
		LitArg lit        -> ppLiteral lit
		NodeArg name args -> Doc.parens (ppNode name (map ppVariable args))

ppLiteral :: Literal -> Doc
ppLiteral literal =
	case literal of
		LiteralInt i -> Doc.integer i
		LiteralString str -> Doc.text (show str)

ppNode :: NodeName -> [Doc] -> Doc
ppNode (ConstructorName constructor) args =
	Doc.hsep (Doc.magenta (ppName constructor) : args)
ppNode (FunctionName fn blanks) args =
	Doc.hsep (Doc.magenta (ppName fn) : args ++ replicate blanks (Doc.char '_'))

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
			Doc.parens (ppNode node (map ppVariable args))
		Write ptr idx arg ->
			ppSyntax "@write" <+>
			ppVariable ptr <> Doc.brackets (Doc.int idx) <+> ppArgument arg
		Address ptr idx ->
			ppSyntax "&" <> ppVariable ptr <> Doc.brackets (Doc.int idx)
		SizeOf node args ->
			ppSyntax "@sizeOf" <+>
			Doc.parens (ppNode node (map ppVariable args))
		Fetch ptr ->
			ppSyntax "@fetch" <+> ppVariable ptr
		Load ptr nth ->
			ppSyntax "@load" <+> ppVariable ptr <> Doc.brackets (Doc.int nth)
		Add lhs rhs ->
			ppSyntax "@add" <+> ppVariable lhs <+> ppVariable rhs
		Application fn args ->
			ppName fn <> Doc.parens (ppList $ map ppVariable args)
		CCall fn args ->
			ppSyntax "@ccall" <+>
			text fn <> Doc.parens (ppList $ map ppVariable args)
		WithExceptionHandler exh exhArgs fn args ->
			ppSyntax "@withExceptionHandler" <+>
			ppName exh <> Doc.parens (ppList $ map ppVariable exhArgs) <+>
			ppName fn <> Doc.parens (ppList $ map ppVariable args)
		Unit arg ->
			ppSyntax "@unit" <> Doc.parens (ppArgument arg)
		Eval var ->
			ppSyntax "@eval" <+> ppVariable var
		Apply a b ->
			ppSyntax "@apply" <+> ppVariable a <+> ppVariable b
		ReadRegister reg ->
			ppSyntax "@register" <+> text reg
		WriteRegister reg var ->
			ppSyntax "@register" <+> text reg <+> Doc.equals <+> ppVariable var
		ReadGlobal reg ->
			ppSyntax "@global" <+> text reg
		WriteGlobal reg var ->
			ppSyntax "@global" <+> text reg <+> Doc.equals <+> ppVariable var
		GCAllocate n ->
			ppSyntax "@gc_allocate" <+> Doc.int n
		GCBegin ->
			ppSyntax "@gc_begin"
		GCEnd ->
			ppSyntax "@gc_end"
		GCMark var ->
			ppSyntax "@gc_mark" <+> ppVariable var
		GCMarkNode var ->
			ppSyntax "@gc_mark_node" <+> ppVariable var
		other -> error $ "PrettyPrint: Unhandled: " ++ show other

ppExpression :: Expression -> Doc
ppExpression expression =
	case expression of
		Return args ->
			ppSyntax "@return" <+>
			ppList (map ppVariable args)
		Bind [] simple rest ->
			ppSimpleExpression simple <> Doc.char ';' Doc.<$$>
			ppExpression rest
		Bind names simple rest ->
			ppList (map ppVariable names) <+>
			text ":=" <+>
			ppSimpleExpression simple <> Doc.char ';' Doc.<$$>
			ppExpression rest
		Case scrut _defaultBranch alts ->
			ppSyntax "case" <+> ppVariable scrut <+> ppSyntax "of" Doc.<$$>
			Doc.indent 2 (Doc.vsep $ map ppAlternative alts)
		Throw obj ->
			ppSyntax "@throw" <+> ppVariable obj
		TailCall fn args ->
			ppSyntax "@tail" <+>
			ppName fn <> Doc.parens (ppList (map ppVariable args))
		Invoke cont args ->
			ppSyntax "@invoke" <>
			Doc.parens (ppList (ppVariable cont : map ppVariable args))
		Exit ->
			ppSyntax "@exit"
		Panic msg ->
			ppSyntax "@panic" <+> text (show msg)

ppSyntax :: String -> Doc
ppSyntax = Doc.green . text

ppFnName :: Name -> Doc
ppFnName = Doc.blue . ppName

ppTypes :: [Type] -> Doc
ppTypes [] = text "void"
ppTypes lst = Doc.hsep $ map ppType lst

ppFunction :: Function -> Doc
ppFunction fn =
	ppTypes (fnResults fn) <+>
	ppFnName (fnName fn) <+> Doc.hsep (map ppVariable (fnArguments fn)) <+>
	Doc.char '=' Doc.<$$>
	Doc.indent 2 (ppExpression (fnBody fn))

ppEntryPoint :: Name -> Doc
ppEntryPoint entry = text "entrypoint:" <+> ppName entry

ppModule :: Module -> Doc
ppModule m =
	Doc.vsep (map ppNodeDefinition (nodes m)) Doc.<$$>
	ppEntryPoint (entryPoint m) Doc.<$$>
	Doc.vsep (map ppFunction (functions m))



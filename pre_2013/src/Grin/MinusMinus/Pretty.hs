-- TODO: Use unicode for the symbols.
module Grin.MinusMinus.Pretty
    ( ppGrin
    ) where

import CompactString
import Grin.MinusMinus.Types

import Text.PrettyPrint.ANSI.Leijen

instance Pretty Grin where
    pretty = ppGrin

ppGrin :: Grin -> Doc
ppGrin grin
    = text "entry:" <+> ppRenamed (grinEntryPoint grin) <$$>
      dullblue (text "Nodes:") <$$>
      vsep (map (ppNodeDef) (grinNodes grin)) <$$>
      dullblue (text "CAFs:") <$$>
      vsep (map (ppCAF) (grinCAFs grin)) <$$>
      dullblue (text "Functions:") <$$>
      vsep (map (ppFuncDef) (grinFunctions grin))

ppNodeDef :: NodeDef -> Doc
ppNodeDef (NodeDef name nodeType args)
    = text "node" <+> ppNodeType nodeType 0 name <+> hsep (map ppType args)

ppType PtrType  = blue (text "*")
ppType WordType = white (text "#")
ppType NodeType = white (text "!")

ppNodeType nt n name
    = green (worker nt n name)
    where worker ConstructorNode 0 name  = char 'C' <> ppRenamed name
          worker ConstructorNode n name  = char 'P' <> int n <> ppRenamed name
          worker FunctionNode 0 name = char 'F' <> ppRenamed name
          worker FunctionNode n name = char 'P' <> int n <> ppRenamed name

ppRenamed (Aliased n var) = pretty var <> char '_' <> pretty n
ppRenamed (Anonymous n)   = char 'x' <> pretty n
ppRenamed (Builtin p)     = char '@' <> pretty p
ppRenamed (External e tys) = parens (text "foreign" <+> text e) -- FIXME: Show types.

ppCAF :: CAF -> Doc
ppCAF (CAF name value)
    = ppRenamed name <+> equals <+> ppValue value

ppFuncDef :: FuncDef -> Doc
ppFuncDef (FuncDef name returns args body)
    = ppRenamed name <> colon <$$>
      indent 2 (ppBeginExpression body)


ppBeginExpression :: Body -> Doc
ppBeginExpression e@(_ :>> _)
    = hang 3 (text "do" <+> ppExpression e)
ppBeginExpression e = ppExpression e


ppExpression :: Body -> Doc
ppExpression (Case value alts)
    = blue (text "case") <+> ppRenamed value <+> blue (text "of") <$$>
      indent 2 (vsep (map ppAlt alts))
ppExpression (a :>> b)
    = ppSExpression a <$$>
      ppExpression b
ppExpression (TailCall fn)
    = blue (text "tailcall") <+> ppRenamed fn
ppExpression Return
    = blue (text "ret")


ppSExpression :: Statement -> Doc
ppSExpression stmt
    = case stmt of
        Constant dst value -> setRegister dst (ppValue value)
        Call fn            -> blue (text "call") <+> ppRenamed fn
        CCall dst fn types args  -> setRegister dst (blue (text "ccall") <+> text fn <+> hsep (map ppRenamed args))
        Store dst size v   -> setRegister dst (blue (text "store") <+> int size <+> ppValues v)
        StoreHole dst n    -> setRegister dst (blue (text "store") <+> hsep (replicate n (text "_")))
        Fetch dst n p      -> setRegister dst (blue (text "fetch") <> brackets (int n) <+> ppRenamed p)
        Move dst src       -> setRegister dst (ppRenamed src)
        Push register      -> blue (text "PUSH") <+> ppRenamed register
        Pop register       -> blue (text "POP") <+> ppRenamed register
        Primop op args     -> blue (pretty op) <+> hsep (map ppRenamed args)
        PrimopSet dst op args -> setRegister dst (blue (pretty op) <+> hsep (map ppRenamed args))
    where setRegister dst val
              = ppRenamed dst <+> text ":=" <+> val

ppAlt (value :> body)
    = ppValue value <+> text "->" <+> ppBeginExpression body

ppValues vals
    = brackets (hsep $ map (ppRenamed) vals)

ppValue (Node name nodeType missing)
    = (ppNodeType nodeType missing name)
ppValue Hole = text "_"
ppValue Empty = text "()"
ppValue (Lit lit) = ppLit lit

ppLit (Lint i) = integer i
ppLit (Lrational r) = text (show r)
ppLit (Lchar char) = text (show char)
ppLit (Lstring string) = text (show string)

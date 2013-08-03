-- TODO: Use unicode for the symbols.
module Grin.Stage2.Pretty
    ( ppGrin
    , ppExpression
    , ppRenamed
    , ppNodeType
    ) where

import CompactString
import Grin.Stage2.Types

import Text.PrettyPrint.ANSI.Leijen

import qualified Data.Map as Map

instance Pretty Grin where
    pretty = ppGrin

ppGrin :: Grin -> Doc
ppGrin grin
    = dullblue (text "Nodes:") <$$>
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
    = hsep (brackets (int returns) <+> ppRenamed name : map (ppRenamed) args) <+> equals <$$>
      indent 2 (ppBeginExpression body)

ppBeginExpression :: Expression -> Doc
ppBeginExpression e@(_ :>>= _)
    = hang 3 (text "do" <+> ppExpression e)
ppBeginExpression e = ppExpression e

ppExpression :: Expression -> Doc
ppExpression (Unit values) = blue (text "unit") <+> ppValues values
ppExpression (Constant value) = blue (text "constant") <+> ppValue value
ppExpression (Case value [cond :> branch])
    = blue (text "guard") <+> ppRenamed value <+> blue equals <+> ppValue cond <$$>
      ppExpression branch
ppExpression (Case value alts)
    = blue (text "case") <+> ppRenamed value <+> blue (text "of") <$$>
      indent 2 (vsep (map (ppAlt) alts))
ppExpression (Application fn args)
    = hsep (ppRenamed fn:map (ppRenamed) args)
ppExpression (Store size v)
    = blue (text "store") <+> int size <+> ppValues v
ppExpression (StoreHole n)
    = blue (text "store") <+> hsep (replicate n (text "_"))
ppExpression (Fetch n p)
    = blue (text "fetch") <> brackets (int n) <+> ppRenamed p
ppExpression (a :>>= [] :-> c)
    = ppExpression a <$$>
      ppExpression c
ppExpression (a :>>= b :-> c)
    = ppValues b <+> text "<-" <+> hang 0 (ppBeginExpression a) <$$>
      ppExpression c

ppAlt (value :> exp) = ppValue value <$$>
                       indent 2 (text "->" <+> align (ppBeginExpression exp))

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

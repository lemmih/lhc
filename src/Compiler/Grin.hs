module Compiler.Grin where

import qualified Language.Haskell.Exts.Annotated as HSE
import Language.Haskell.Names

import Text.PrettyPrint.ANSI.Leijen


--------------------------------------------------
-- Types

data Ident = Aliased String | Anonymous Int

data Lit =
  Lint Integer |
  Lrational Rational |
  Lchar Char |
  Lstring String
  deriving ( Show )

data Value =
  Empty |
  Lit Lit |
  Variable Ident

data Lambda = Ident :-> Expression

data Alt = Value :> Expression

infixr 1 :->
infixr 1 :>>=
infixr 1 :>>


data Expression
  = Expression :>>= Lambda
  | Expression :>> Expression
  | Application Ident [Ident]
  | Case Ident [Alt]
  | Store Int Value
  | Update Int Ident Ident
  | Unit Value
  | Eval Ident
  | Apply Ident Ident


data NodeDef = NodeDef
  { nodeName :: Ident
  , nodeType :: NodeType
  , nodeArgs :: [Type]
  }

data NodeType
  = ConstructorNode
  | FunctionNode

data Type = PtrType | WordType | NodeType

data FuncDef = FuncDef
  { funcDefName :: Ident
  , funcDefArgs :: [Ident]
  , funcDefBody :: Expression
  }


data Grin = Grin
  { grinNodes :: [NodeDef]
  , grinFunctions :: [FuncDef]
  }



--------------------------------------------------
-- Pretty print

ppGrin :: Grin -> Doc
ppGrin grin =
  dullblue (text "Nodes:") <$$>
  vsep (map ppNodeDef (grinNodes grin)) <$$>
  dullblue (text "Functions:") <$$>
  vsep (map ppFuncDef (grinFunctions grin))

ppNodeDef :: NodeDef -> Doc
ppNodeDef (NodeDef name nodeType args)
    = text "node" <+> ppNodeType nodeType 0 name <+> hsep (map ppType args)


ppFuncDef :: FuncDef -> Doc
ppFuncDef (FuncDef name args body)
    = hsep (ppIdent name : map (ppIdent) args) <+> equals <$$>
      indent 2 (ppBeginExpression body)

ppBeginExpression :: Expression -> Doc
ppBeginExpression e@(_ :>>= _)
    = hang 3 (text "do" <+> ppExpression e)
ppBeginExpression e = ppExpression e

ppExpression :: Expression -> Doc
ppExpression (Unit value) = blue (text "unit") <+> ppValue value
ppExpression (Case value alts)
    = blue (text "case") <+> ppValue (Variable value) <+> blue (text "of") <$$>
      indent 2 (vsep (map (ppAlt) alts))
ppExpression (Application fn args)
    = hsep (ppIdent fn:map (ppIdent) args)
ppExpression (Update size ptr val)
    = blue (text "update") <+> int size <+> ppIdent ptr <+> ppIdent val
ppExpression (Store size v)
    = blue (text "store") <+> int size <+> ppValue v
ppExpression (Eval a)
    = blue (text "eval") <+> ppIdent a
ppExpression (Apply a b)
    = blue (text "apply") <+> ppIdent a <+> ppIdent b
ppExpression (a :>> c)
    = ppExpression a <$$>
      ppExpression c
ppExpression (a :>>= b :-> c)
    = ppValue (Variable b) <+> text "<-" <+> hang 0 (ppBeginExpression a) <$$>
      ppExpression c

ppAlt (value :> exp) = ppValue value <$$>
                            indent 2 (text "->" <+> align (ppBeginExpression exp))

--ppValue (Node name nodeType missing args)
--    = parens (hsep (ppNodeType nodeType missing name : map (ppIdent) args))
--ppValue (Vector vs) = brackets (hsep (map (ppIdent) vs))
--ppValue (Hole size) = parens (text "@hole" <+> hsep (replicate size (char '_')))
ppValue Empty = text "()"
ppValue (Lit lit) = ppLit lit
ppValue (Variable variable) = ppIdent variable

ppLit (Lint i) = integer i
ppLit (Lrational r) = text (show r)
ppLit (Lchar char) = text (show char)
ppLit (Lstring string) = text (show string)




ppType PtrType  = blue (text "*")
ppType WordType = white (text "#")
ppType NodeType = white (text "!")

ppNodeType nt n name
    = green (worker nt n name)
    where worker ConstructorNode 0 name  = char 'C' <> ppIdent name
          worker ConstructorNode n name  = char 'P' <> int n <> ppIdent name
          worker FunctionNode 0 name = char 'F' <> ppIdent name
          worker FunctionNode n name = char 'P' <> int n <> ppIdent name


ppIdent :: Ident -> Doc
ppIdent (Aliased str) = text str
ppIdent (Anonymous n) = text "n_" <> int n



--------------------------------------------------
-- Conversion

data Context = Strict | Lazy

toGrin :: HSE.Module (Scoped HSE.SrcSpan) -> Grin
toGrin (HSE.Module _ _head _pragma _imports decls) = Grin
  { grinNodes     = concatMap toNodeDefs decls
  , grinFunctions = concatMap toFuncDef decls
  }

toNodeDefs :: HSE.Decl (Scoped l) -> [NodeDef]
toNodeDefs (HSE.GDataDecl _ _dataOrNew _context _head _kind decls _) =
  [ NodeDef (identFromName name) ConstructorNode (argsFromType ty)
  | HSE.GadtDecl _ name ty <- decls ]
toNodeDefs _ = []


argsFromType :: HSE.Type l -> [Type]
argsFromType (HSE.TyFun _ _ b) = PtrType : argsFromType b
argsFromType _ = []

identFromName :: HSE.Name (Scoped l) -> Ident
identFromName (HSE.Ident (Scoped info _) txt)   = identFromNameInfo info txt
identFromName (HSE.Symbol (Scoped info _) txt) = identFromNameInfo info txt

identFromNameInfo :: NameInfo l -> String -> Ident
identFromNameInfo (GlobalValue sym) _ = Aliased (ppOrigName (sv_origName sym))
identFromNameInfo (GlobalType sym) _  = Aliased (ppOrigName (st_origName sym))
--identFromNameInfo None txt = Aliased "none"
identFromNameInfo other txt = Aliased txt





toFuncDef :: Show l => HSE.Decl (Scoped l) -> [FuncDef]
toFuncDef (HSE.FunBind _ [HSE.Match _ name pats (HSE.UnGuardedRhs _ exp) _]) =
  [ FuncDef
      (identFromName name)
      [ identFromName var | HSE.PVar _ var <- pats ]
      (expToBody exp)
  ]
toFuncDef (HSE.PatBind _ (HSE.PVar _ var) _ (HSE.UnGuardedRhs _ exp) _) =
  [ FuncDef
      (identFromName var)
      []
      (expToBody exp)
  ]
toFuncDef _ = []

expToBody :: Show l => HSE.Exp (Scoped l) -> Expression
expToBody (HSE.Var _ qname) = Unit (Variable (identFromQName qname))
expToBody (HSE.App _ a b) =

expToBody exp@HSE.App{} =
  let (e, vars) = gatherApp exp
  in Application e vars
expToBody _ = Unit Empty

gatherApp :: Show l => HSE.Exp (Scoped l) -> (Ident, [Ident])
gatherApp = worker []
  where
    conv (fn,args) = (identFromQName fn, map identFromQName args)
    worker args (HSE.App _ l (HSE.Var _ qname)) = worker (qname:args) l
    worker args (HSE.Var _ qname) = conv (qname, reverse args)
    worker args (HSE.Con _ qname) = conv(qname, reverse args)
    worker args _ = (Aliased "missing", [])
    worker args other = error $ "gatherApp:\n" ++ show other

identFromQName :: HSE.QName (Scoped l) -> Ident
identFromQName (HSE.Qual _ _ name) = identFromName name
identFromQName (HSE.UnQual _ name) = identFromName name
identFromQName (HSE.Special _ special) =
  case special of
    HSE.UnitCon _ -> Aliased "()"
    HSE.ListCon _ -> Aliased "[]"
    HSE.FunCon _  -> Aliased "(->)"
    HSE.TupleCon _ _ n -> Aliased "tuple"
    HSE.Cons _ -> Aliased "(:)"
    _ -> Aliased "other_special"




















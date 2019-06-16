module Tigr.Pretty where

import Tigr.Types
import Text.PrettyPrint.ANSI.Leijen hiding (Pretty(..))

{-
data GCode
  = Var Name [Name]
  | Con Name [Name]
  | External String [Name]
  | Let Name [Name] GCode GCode
  | LetRec [(Name, [Name], GCode)] GCode
  | LetStrict Name GCode GCode
  | Lit Literal
  | Case Name (Maybe GCode) [Alt]
  | Lam [Name] [Name] GCode
  | Throw Name
  | Catch [Name] Name GCode GCode
    deriving (Show)

data Alt = Alt Pattern GCode
  deriving (Show)

data Pattern
  = ConPattern Name [Name]
  | LitPattern Literal
    deriving (Show)
-}
ppGCode :: GCode -> Doc
ppGCode gcode =
  case gcode of
    Var fn args -> text fn <+> list (map text args)
    Con con args -> text con <+> list (map text args)
    External cFn args -> text "external" <+> text cFn <+> list (map text args)
    Let bind free rhs body ->
      text "let" <+> text bind <+> equals <+> ppFree free <$$>
        indent 6 (ppGCode rhs) <$$>
      ppGCode body
    LetRec binds body ->
      text "letrec" <$$>
        indent 2 (vsep [ text bind <+> equals <+> ppFree free <$$>
                         indent 6 (ppGCode rhs) | (bind, free, rhs) <- binds ]) <$$>
      ppGCode body
    LetStrict bind rhs body ->
      text "let!" <+> text bind <+> equals <+> hang 0 (ppGCode rhs) <$$>
      ppGCode body
    Lit lit -> ppLit lit
    Case scrut Nothing alts ->
      text "case" <+> text scrut <+> text "of" <$$>
      indent 2 (vsep $ map ppAlt alts)
    Case scrut (Just def) alts ->
      text "case" <+> text scrut <+> text "of" <$$>
      indent 2 (vsep $ map ppAlt alts ++ [text "DEFAULT" <+> text "->" <$$> indent 2 (ppGCode def)])
    Lam free binds body ->
      ppFree free <+> text "\\" <> hsep (map text binds) <+> text "->" <$$>
      indent 2 (ppGCode body)
    Throw eh -> text "Throw" <+> text eh
    Catch free bind handler body -> undefined

ppAlt :: Alt -> Doc
ppAlt (Alt (ConPattern con args) branch) =
  text con <+> hsep (map text args) <+> text "->" <$$>
  indent 2 (ppGCode branch)
ppAlt (Alt (LitPattern lit) branch) =
  ppLit lit <+> text "->" <$$>
  indent 2 (ppGCode branch)

ppFree :: [Name] -> Doc
-- ppFree = list . map text
ppFree = angles . hcat . punctuate (text ", ") . map text

ppLit :: Literal -> Doc
ppLit (LiteralI64 i) = int i
ppLit (LiteralString s) = text (show s)

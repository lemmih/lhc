{-# LANGUAGE OverloadedStrings #-}
module Grin.Stage2.Backend.LLVM ( compile ) where

import qualified Grin.Stage2.Types as Grin
import Grin.Stage2.Types
import CompactString

import Control.Monad.State
import Control.Monad.Reader
import System.FilePath
import System.Directory
import Text.PrettyPrint.ANSI.Leijen hiding ((</>), (<$>))
import qualified Data.Map as Map
import Data.Char
import Text.Printf
import Control.Applicative hiding (empty)

import Paths_lhc

type Scope = Map.Map Renamed LLVMValue

data LLVMValue = Local String | Global String | StringGlobal String

compile :: Grin -> FilePath -> IO ()
compile grin target
    = do rts <- getDataFileName ("rts" </> "rts.ll")
         let llvmTarget = replaceExtension target "ll"
         copyFile rts llvmTarget
         appendFile llvmTarget (show (grinToLLVM grin))

grinToLLVM :: Grin -> Doc
grinToLLVM grin = runReader (toLLVM grin) grin

type M a = Reader Grin a

toLLVM :: Grin -> M Doc
toLLVM grin
    = do cafs <- vsep <$> mapM cafToLLVM (grinCAFs grin)
         funcs <- vsep <$> mapM funcDefToLLVM (grinFunctions grin)
         return $ vsep [ comment "CAFs:"
                       , cafs
                       , comment "Functions:"
                       , funcs ]

cafToLLVM :: CAF -> M Doc
cafToLLVM CAF{cafName = name, cafValue = Lit (Lstring str)}
    = return $ text "@" <> ppRenamed name <+> equals <+> stringConstant (str++"\0")
cafToLLVM CAF{cafName = name, cafValue = Node{}}
    = return $ text "@" <> ppRenamed name <+> equals <+> global <+> unitp <+> zeroinitializer
cafToLLVM caf = return $ comment "FIXME: cafToLLVM"

funcDefToLLVM :: FuncDef -> M Doc
funcDefToLLVM func
    = body >>= \body' -> return $
      define <+> void <+> char '@' <> ppRenamed (funcDefName func) <> parens argList <+> lbrace <$$>
      indent 2 ( rets <$$> body' <$$> exit) <$$>
      rbrace
    where retArg n = char '%' <> text "ret_" <> int n
          rets = vsep [ retArg n <+> equals <+> call <+> unitp <+> text "@getReturnValuePtr" <> parens (i32 <+> int n ) | n <- [0.. funcDefReturns func - 1] ]
          body = expressionToLLVM (map retArg [0 .. funcDefReturns func - 1]) (funcDefBody func)
          exit = ret <+> void
          argList = hang 0 $ sep $ punctuate comma [ unitp <+> char '%' <> ppRenamed arg | arg <- funcDefArgs func ]
{-
data LLVMDecl
    = LLVMGlobal Renamed
    | LLVMConstant Renamed String
    | LLVMFunction Renamed [LLVMVar]

data LLVMVar
    = LLVMLocal Renamed
    | LLVMGlobal Renamed
    | LLVMGlobalString Renamed

data LLVMStmt
    = LLVMStore LLVMVar LLVMVar
    | LLVMBind LLVMVar LLVMExpression
    | LLVMComment String
data LLVMExpression
    = LLVMLoad LLVMVar
    | LLVMAlloca LLVMType
-}
expressionToLLVM :: [Doc] -> Expression -> M Doc
expressionToLLVM binds exp 
    = case exp of
       Constant (Lit (Lint i)) -> return $ storeUnit (int $ fromIntegral i) (head binds)
       a :>>= binds' :-> b     -> do let llvmBinds = [ char '%' <> ppRenamed var | var <- binds' ]
                                     a' <- expressionToLLVM llvmBinds a
                                     b' <- expressionToLLVM binds b
                                     return $ vsep [ var <+> equals <+> alloca unit | var <- llvmBinds ] <$$>
                                              a' <$$> b'
       Unit vars               -> return $ vsep [ ppTemp bind (ppRenamed var) <+> equals <+> text "load" <+> unitp <+> char '%' <> ppRenamed var <$$>
                                                  storeUnit (ppTemp bind (ppRenamed var)) bind
                                                  | (var,bind) <- zip vars binds ]
       Application (Builtin "realWorld#") []
                               -> return $ storeUnit (int 0) (head binds)
       Application (Builtin "unreachable") []
                               -> return $ text "unreachable"
       Application fn args | not (isBuiltin fn) && not (isExternal fn)
         -> let argList = hang 0 $ sep $ punctuate comma $ [ unitp <+> char '%' <> ppRenamed arg | arg <- args ]
            in return $ call <+> void <+> char '@' <> ppRenamed fn <> parens argList
       _                       -> return $ comment "FIXME: expressionToLLVM"

-------------------------------------------------------------
-- Utilities

storeUnit val ptr = text "store" <+> unit <+> val <> text "," <+> unitp <+> ptr
alloca ty = text "alloca" <+> ty

ppTemp a b = char '%' <> text "tmp_" <> text (drop 1 $ show a) <> b
comment str = char ';' <+> text str

ret = text "ret"
void = text "void"
define = text "define"
call = text "call"
i32 = text "i32"
i32p = text "i32*"
unit = text "%unit"
unitp = text "%unit*"
global = text "global"
zeroinitializer = text "zeroinitializer"

stringConstant str
    = internal <+> constant <+> strType <+> char 'c' <> dquotes (escString str)
    where strType = brackets (int (length str) <+> char 'x' <+> text "i8")
          internal = text "internal"
          constant = text "constant"

escString :: String -> Doc
escString string = text (concatMap worker string)
    where worker c | isPrint c = [c]
                   | otherwise = printf "\\%02x" (ord c)

ppRenamed :: Renamed -> Doc
ppRenamed (Anonymous i)
    = text "anon_" <> int i
ppRenamed (Aliased i name)
    = text "named_" <> sanitize name <> char '_' <> int i
ppRenamed (Builtin "undefined")
    = text "0"
ppRenamed (Builtin builtin)
    = error $ "Grin.Stage2.Backend.LLVM.ppRenamed: Unknown primitive: " ++ show builtin

sanitize :: CompactString -> Doc
sanitize cs = text (map sanitizeChar $ show $ pretty cs)

sanitizeChar :: Char -> Char
sanitizeChar c | isAlphaNum c = c
               | otherwise    = '_'

{-
data Var
    = GlobalVar Renamed Type
    | LocalVar  Int Type
    | IntConstant Int    Type
    | StrConstant String Type

varType :: Var -> Type
varType (GlobalVar _name ty)  = ty
varType (LocalVar _ident ty)   = ty
varType (IntConstant _val ty) = ty
varType (StrConstant _str ty) = ty

data Type
    = Word
    | I8
    | Pointer Type
    | Array Int Type
    | Struct [Type]
    | Named String
    deriving (Show)

-- ret type
data Function
    = Function Renamed [Var] [Statement]

data Statement
    = Assignment Var Expression
    | Comment String
    | VoidCall Renamed [Var]
    | Ret Var
    | RetVoid
    | RetArray Type [Var]
    | RetStruct [Var]
    | Store Var Var
data Expression
    = BinOp Var BinOp Var
    | Cast Var Type
    | Load Var
    | Call Type Renamed [Var]
    | GetElementPtr Type Var [Int]
    | ExtractValue Type Var Int

data BinOp
    = Add
    | Shl
    | Shr
    | And
    | Or

data Module
    = Module { moduleGlobals :: [(Var, Value)]
             , moduleFunctions :: [Function]
             }

type Scope = Map.Map Renamed Var
type ReturnArity = Map.Map Renamed Int
type M a = ReaderT (Scope, ReturnArity) (State Int) a

fixedSize :: Int
fixedSize = 10

memT :: Type
memT = Named "mem"

returnArgs = [ GlobalVar (Anonymous n) (Pointer Word) | n <- [1..10] ]


fromGrin :: Grin.Grin -> Module
fromGrin grin
    = let scope = (Map.empty, returnArity)
          returnArity = Map.fromList [ (funcDefName func, funcDefReturns func) | func <- grinFunctions grin ]
          unique = grinUnique grin
          genFunctions = extendScope (zip (map cafName (grinCAFs grin)) cafVars) $
                         mapM fromFuncDef (grinFunctions grin)
          functions = evalState (runReaderT genFunctions scope) unique
          cafVars = [ GlobalVar (cafName caf) (Pointer memT) | caf <- grinCAFs grin ]
      in Module { moduleGlobals   = [ (GlobalVar (cafName caf) memT, uniqueId (cafName caf):replicate (fixedSize-1) 0 ) | caf <- grinCAFs grin]
                , moduleFunctions = functions
                }

fromFuncDef :: FuncDef -> M Function
fromFuncDef funcDef
    = do args' <- replicateM (length (funcDefArgs funcDef)) (newVariable Word)
         extendScope (zip (funcDefArgs funcDef) args') $
           do rets <- replicateM (funcDefReturns funcDef) $ newVariable Word
              let setReturnArgs = [ Store ret var | (var, ret) <- zip returnArgs rets]
              stmts <- fromExpression rets (funcDefBody funcDef)
              return $ Function (funcDefName funcDef) args' (stmts ++ setReturnArgs ++ [RetVoid])

fromExpression :: [Var] -> Grin.Expression -> M [Statement]
fromExpression [bind] (Grin.Constant value)
    = return [Assignment bind (valueToExpression (varType bind) value)]

fromExpression [bind] (Grin.Application (Builtin "realWorld#") [])
    = return [Assignment bind $ Cast (IntConstant 0 Word) (varType bind)]

fromExpression [bind] (Grin.Fetch nth renamed)
    = do var <- lookupVariable renamed
         castedVar <- newVariable (Pointer Word)
         ptrVar <- newVariable (Pointer Word)
         return [ Comment $ show $ text "fetch" <> brackets (int nth) <+> ppRenamed renamed
                , Assignment castedVar $ Cast var (Pointer Word)
                , Assignment ptrVar $ GetElementPtr (varType castedVar) castedVar [nth]
                , Assignment bind   (Load ptrVar)
                ]

fromExpression binds (Grin.Application fn args) | not (isBuiltin fn) && not (isExternal fn)
    = do arity <- lookupReturnArity fn
         args' <- mapM lookupVariable args
         let funcType = Struct (replicate arity Word)
         return $ [ VoidCall fn args' ] ++
                  [ Assignment bind $ Load var | (var,bind) <- zip returnArgs binds ]


fromExpression binds (a Grin.:>>= binds' Grin.:-> b)
    = do args <- replicateM (length binds') $ newVariable Word
         a' <- fromExpression args a
         extendScope (zip binds' args) $
           do b' <- fromExpression binds b
              return (a' ++ b')
fromExpression _binds _ = return []

valueToExpression :: Type -> Grin.Value -> Expression
valueToExpression ty (Lit (Lint i))  = Cast (IntConstant (fromIntegral i) Word) ty
valueToExpression ty (Lit (Lchar c)) = Cast (IntConstant (ord c) Word) ty
valueToExpression ty (Lit (Lstring s)) = Cast (StrConstant s (Array (length s + 1) I8)) ty
valueToExpression ty (Node tag nt missing)
    = Cast (IntConstant (uniqueId tag) Word) ty
valueToExpression ty (Empty)
    = Cast (IntConstant 0 Word) ty
valueToExpression ty (Hole)
    = Cast (IntConstant 0 Word) ty



extendScope :: [(Renamed, Var)] -> M a -> M a
extendScope assocs
    = local $ \(scope,returnArity) -> (Map.fromList assocs `Map.union` scope, returnArity)

newVariable :: Type -> M Var
newVariable ty
    = do u <- get
         put (u+1)
         return $ LocalVar u ty

lookupVariable :: Renamed -> M Var
lookupVariable variable
    = asks $ Map.findWithDefault errMsg variable . fst
    where errMsg = error $ "Grin.Stage2.Backend.LLVM.lookupVariable: couldn't find key: " ++ show variable

lookupReturnArity :: Renamed -> M Int
lookupReturnArity function
    = asks $ Map.findWithDefault errMsg function . snd
    where errMsg = error $ "Grin.Stage2.Backend.LLVM.lookupReturnArity: couldn't find key: " ++ show function








ppModule :: Module -> Doc
ppModule llvmModule
    = ppNamedType "mem" (Array fixedSize Word) <$$>
      ppComment "return arguments:" <$$>
      vsep (map ppReturnArg returnArgs) <$$>
      ppComment "CAFs:" <$$>
      vsep (map ppGlobal (moduleGlobals llvmModule)) <$$>
      ppComment "Functions:" <$$>
      vsep (map ppFunction (moduleFunctions llvmModule))

ppFunction :: Function -> Doc
ppFunction (Function name args stmts)
    = text "define" <+> text "void" <+> char '@' <> ppRenamed name <>
      parens (hsep (punctuate comma [ppType (varType var) <+> ppVar var | var <- args])) <+>
      braces (linebreak <>
              indent 2 (vsep $ map ppStatement stmts) <>
              linebreak)

ppStatement :: Statement -> Doc
ppStatement (Ret var)
    = text "ret" <+> ppType (varType var) <+> ppVar var
ppStatement (RetVoid)
    = text "ret" <+> text "void"
ppStatement (RetArray ty vars)
    = text "ret" <+> ppType ty <+> brackets (hsep $ punctuate comma $ [ ppType (varType var) <+> ppVar var | var <- vars])
ppStatement (RetStruct vars)
    = text "ret" <+> ppType (Struct (map varType vars)) <+> braces (hsep $ punctuate comma $ [ ppType (varType var) <+> ppVar var | var <- vars])
ppStatement (Comment str)
    = ppComment str
ppStatement (Assignment var exp)
    = ppVar var <+> equals <+> ppExpression exp
ppStatement (VoidCall fn args)
    = text "call" <+> text "void" <+> char '@' <> ppRenamed fn <> parens (hsep (punctuate comma $ [ppType (varType arg) <+> ppVar arg | arg <- args]))
ppStatement (Store var ptr)
    = text "store" <+> ppType (varType var) <+> ppVar var <> comma <+> ppType (varType ptr) <+> ppVar ptr

ppExpression (Cast var ty)
    = case (varType var, ty) of
        (Pointer{}, Pointer{}) -> bitcast
        (Pointer{}, _)         -> ptrtoint
        (_, Pointer{})         -> inttoptr
        _                      -> bitcast
    where bitcast  = text "bitcast"  <+> ppType (varType var) <+> ppVar var <+> text "to" <+> ppType ty
          ptrtoint = text "ptrtoint" <+> ppType (varType var) <+> ppVar var <+> text "to" <+> ppType ty
          inttoptr = text "inttoptr" <+> ppType (varType var) <+> ppVar var <+> text "to" <+> ppType ty
ppExpression (Load ptr)
    = text "load" <+> ppType (varType ptr) <+> ppVar ptr
ppExpression (BinOp a op b)
    = ppBinOp op <+> ppType (varType a) <+> ppVar a <> comma <+> ppVar b
ppExpression (Call ty fn args)
    = text "call" <+> ppType ty <+> char '@' <> ppRenamed fn <> parens (hsep (punctuate comma $ [ppType (varType arg) <+> ppVar arg | arg <- args]))
ppExpression (GetElementPtr ty var idx)
    = text "getelementptr" <+> ppType ty <+> ppVar var <> comma <+> hsep (punctuate comma $ [ text "i32" <+> int nth | nth <- idx ])
ppExpression (ExtractValue ty var idx)
    = text "extractvalue" <+> ppType ty <+> ppVar var <> comma <+> int idx

ppBinOp Add = text "add"

ppNamedType :: String -> Type -> Doc
ppNamedType synonym ty
    = char '%' <> text synonym <+> equals <+> text "type" <+> ppType ty

ppComment :: String -> Doc
ppComment comment = char ';' <+> text comment

ppReturnArg :: Var -> Doc
ppReturnArg var
    = ppVar var <+> equals <+> text "global" <+> ppType Word <+> int 0

ppGlobal :: (Var,Value) -> Doc
ppGlobal (var,Node tag _nt _missing)
    = ppVar var <+> equals <+> text "global" <+> ppType (varType var) <+> ppArray Word initValues

ppArray :: Type -> [Int] -> Doc
ppArray ty vals = brackets (hsep $ punctuate comma $ [ ppType ty <+> int val | val <- vals ])

ppType :: Type -> Doc
ppType (Word) = text "i64"
ppType I8     = text "i8"
ppType (Pointer ty) = ppType ty <> char '*'
ppType (Array size eltType)
    = brackets (int size <+> char 'x' <+> ppType eltType)
ppType (Struct tys)
    = braces $ hsep $ punctuate comma $ map ppType tys
ppType (Named name) = char '%' <> text name

ppVar :: Var -> Doc
ppVar (GlobalVar name _ty) = char '@' <> ppRenamed name
ppVar (LocalVar ident _ty) = char '%' <> text "local_" <> int ident
ppVar (IntConstant i  _ty) = int i
ppVar (StrConstant str _ty) = char 'c' <> text (show str)

ppRenamed :: Renamed -> Doc
ppRenamed renamed
    = case alias renamed of
        Just name -> text $ show name
        Nothing   -> text $ show $ show $ uniqueId renamed

-}



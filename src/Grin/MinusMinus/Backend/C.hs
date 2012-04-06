{-# LANGUAGE OverloadedStrings #-}
module Grin.MinusMinus.Backend.C where

import CompactString
import Grin.MinusMinus.Types

import Text.PrettyPrint.ANSI.Leijen hiding ((</>))
import Data.Char
import Text.Printf
import System.Process
import System.FilePath
import Control.Monad
import System.IO
import System.Directory
import System.Exit

import Paths_lhc

import Config

compile :: Config -> Grin -> FilePath -> IO ()
compile cfg
    | configDebugBuild cfg  = compile' cfg ["--debug", "-ggdb"]
    | configProfBuild cfg  = compile' cfg ["-pg", "-O2"]
    | otherwise             = compile' cfg ["-O"++show 2]

compile' :: Config -> [String] -> Grin -> FilePath -> IO ()
compile' cfg gccArgs grin target
    = do let cTarget = replaceExtension target "c"
                      
         dDir <- getDataDir
         let rtsDir = dDir </> "rts"
         rtsFiles <- getCFiles rtsDir
         
         let ltmDir = rtsDir </> "ltm"
         ltmFiles <- getCFiles ltmDir
         
         let rtsOptions = ["-I"++rtsDir]
         let ltmOptions = ["-I"++ltmDir, "-DXMALLOC=GC_malloc", "-DXFREE=GC_free", "-DXREALLOC=GC_realloc"]
         let fullOpts = unwords $ ccLine ++ rtsOptions ++ ltmOptions ++ ltmFiles ++ rtsFiles ++ [cTarget]
         
         writeFile cTarget (show $ ppGrin (configOpt cfg) fullOpts grin)
         
         pid <- runCommand fullOpts
         ret <- waitForProcess pid
         case ret of
           ExitSuccess -> return ()
           _ -> do hPutStrLn stderr "C code failed to compile."
                   exitWith ret
    where getCFiles dir = (map (dir </>) . filter (\f -> takeExtension f == ".c")) `liftM` (getDirectoryContents dir)
          ccLine = [cCompiler
                   , "-DLHC_GC_BLOCK_SIZE="++show (optGCBlockSize (configOpt cfg))
                   , "-DLHC_GC_GRACE_BUFFER="++show (optSmallNodeSize (configOpt cfg))
                   , "-w", "-lm", "-I/usr/include/gc/", "-lgc", "-o", target] ++ gccArgs
          cCompiler = configUseCc cfg


ppGrin :: OptConfig -> String -> Grin -> Doc
ppGrin cfg cmdLine grin
    = vsep [ text "#include \"master.h\""
           , text $ "char* lhc_cc_compile = \"" ++ cmdLine ++ "\";"
           , comment "Registers"
           , ppRegisters grin
           , comment "Prototypes:"
           , ppPrototypes grin
           , comment "Functions:"
           , ppFunctions grin
           , comment "Main:"
           , ppHsMain cfg (grinCAFs grin) (grinEntryPoint grin)
           , line
           ]

ppRegisters :: Grin -> Doc
ppRegisters grin
    = unitp <+> text "registers" <> brackets (int $ grinUnique grin) <> semi -- unit* registers[n];

ppRegister :: Renamed -> Doc
ppRegister identifier
    = case identifier of
        Anonymous i -> text "registers" <> brackets (int i)
        Aliased i _ -> text "registers" <> brackets (int i)
        Builtin "undefined" -> int 0
        _           -> error $ "Grin.MinusMinus.Backend.C.ppRegister: Unknown identifier: " ++ show identifier


unitSize = 8

ppHsMain :: OptConfig -> [CAF] -> Renamed -> Doc
ppHsMain cfg cafs entryPoint
    = text "void" <+> text "__hs_xmain() " <+> char '{' <$$>
      indent 2 ( vsep [ vsep [ ppRegister name <+> equals <+> alloc (int (nodeSize * unitSize)) <> semi
                             , ppRegister name <> brackets (int 0) <+> equals <+> int (uniqueId tag) <> semi]
                        | CAF{cafName = name, cafValue = Node tag _nt _missing} <- cafs ] <$$>
                 vsep [ vsep [ ppRegister name <+> equals <+> cunitp  <+> escString (str ++ "\0") <> semi]
                        | CAF{cafName = name, cafValue = Lit (Lstring str)} <- cafs ] <$$>
                 ppRenamed entryPoint <> parens empty <> semi) <$$>
      char '}'
    where nodeSize = optSmallNodeSize cfg


ppPrototypes :: Grin -> Doc
ppPrototypes grin = vsep [ ppPrototype func | func <- grinFunctions grin ]

ppPrototype :: FuncDef -> Doc
ppPrototype func
    = void <+> ppRenamed (funcDefName func) <> parens empty <> semi

ppFunctions :: Grin -> Doc
ppFunctions grin = vsep [ ppFuncDef func | func <- grinFunctions grin ]

ppFuncDef :: FuncDef -> Doc
ppFuncDef func
    = void <+> ppRenamed (funcDefName func) <> parens empty <+> lbrace <$$>
      indent 2 (ppBody (funcDefBody func)) <$$>
      rbrace

ppBody :: Body -> Doc
ppBody body
    = case body of
        Return           -> text "return;"
        TailCall label   -> call label
        stmt :>> next    -> ppStatement stmt <$$> ppBody next
        Case scrut alts  -> ppCase scrut alts

ppCase scrut []
    = error $ "Empty case: " ++ show scrut
ppCase scrut alts
    = switch (cunit <+> ppRegister scrut) $
        vsep (map ppAlt alts) <$$> def (last alts)
    where def (Empty :> _) = empty
          def _            = text "default:" <$$> indent 2 (panic ("No match for case: " ++ show scrut))
          ppAlt (value :> body)
              = case value of
                  Empty
                    -> text "default:" <$$> braces rest
                  Node tag _nt _missing
                    -> text "case" <+> int (uniqueId tag) <> colon <$$> braces rest
                  Lit (Lint i)
                    -> text "case" <+> int (fromIntegral i) <> colon <$$> braces rest
                  Lit (Lchar c)
                    -> text "case" <+> int (ord c) <> colon <$$> braces rest
              where rest = ppBody body

ppStatement :: Statement -> Doc
ppStatement statement
    = case statement of
        Call fn             -> call fn
        CCall dst fn types args -> ppExternal dst fn types args
        Fetch dst idx src   -> ppRegister dst <+> equals <+> parens (ppRegister src) <> brackets (int idx) <> semi
        Store dst size regs -> vsep [ ppRegister dst <+> equals <+> alloc (int (size*unitSize)) <> semi
                                    , vsep [ parens (ppRegister dst) <> brackets (int n) <+> equals <+> ppRegister reg <> semi | (n, reg) <- zip [0..] regs ]
                                    ]
        StoreHole dst size  -> ppRegister dst <+> equals <+> alloc (int (size*unitSize)) <> semi
        Move dst src        -> ppRegister dst <+> equals <+> ppRegister src <> semi
        Constant dst val    -> ppRegister dst <+> equals <+> ppValue val <> semi
        Push reg            -> empty -- text "PUSH" <> parens (ppRegister reg) <> semi
        Pop reg             -> empty -- ppRegister reg <+> equals <+> text "POP" <> semi
        Primop prim args    -> voidPrimops prim args
        PrimopSet dst prim args -> ppRegister dst <+> equals <+> setPrimops prim args

ppExternal :: Renamed -> String -> [FFIType] -> [Renamed] -> Doc
ppExternal dst fn tys args
    | returnType == UnitType
    = call
    | otherwise
    = ppRegister dst <+> equals <+> call
    where returnType = last tys
          call = text fn <> parens (hsep $ punctuate comma (map ppRegister args)) <> semi

voidPrimops :: CompactString -> [Register] -> Doc
voidPrimops prim ~args@(a : ~(b : ~(c : ~(d : ~[]))))
    = case prim of
        "update" -> vsep [ writeArray a n value | (n,value) <- zip [0..] (tail args) ]
        "writeInt8OffAddr#" -> writeAnyArray s8 a b c
        "writeWideCharOffAddr#" -> writeAnyArray s32 a b c
        "writeAddrOffAddr#" -> writeAnyArray unit a b c
        "writeWord64OffAddr#" -> writeAnyArray u64 a b c
        "touch#" -> empty
        "updateMutVar"      -> text "updateMutVar" <> parens (hsep $ punctuate comma [ppRegister a, ppRegister b]) <> semi
        _other   -> panic $ "Unhandled void primop: " ++ show prim
    where writeArray arr nth val
              = ppRegister arr <> brackets (int nth) <+> equals <+> cunit <+> ppRegister val <> semi
          writeAnyArray ty arr idx elt
              = parens (parens (ty <> char '*') <+> ppRegister arr) <> brackets (cunit <+> ppRegister idx)
                <+> equals <+>
                parens ty <+> cunit <+> ppRegister elt <> semi



setPrimops :: CompactString -> [Register] -> Doc
setPrimops prim ~(a : ~(b : ~(c: ~(d : ~[]))))
    = case prim of
        "realWorld#"        -> int 0 <> semi
        "noDuplicate#"      -> ppRegister a <> semi
        "indexCharOffAddr#" -> indexAnyArray cu8p a b <> semi

        "*#"                -> binOp csunit "*"
        "+#"                -> binOp csunit "+"
        "-#"                -> binOp csunit "-"
        "quotInt#"          -> binOp csunit "/"
        "remInt#"           -> binOp csunit "%"
        "negateInt#"        -> unOp csunit "-"

        "==#"               -> cmpOp csunit "=="
        "/=#"               -> cmpOp csunit "!="
        ">#"                -> cmpOp csunit ">"
        ">=#"               -> cmpOp csunit ">="
        "<#"                -> cmpOp csunit "<"
        "<=#"               -> cmpOp csunit "<="

        "eqWord#"           -> cmpOp cunit "=="
        "neWord#"           -> cmpOp cunit "!="
        "gtWord#"           -> cmpOp cunit ">"
        "geWord#"           -> cmpOp cunit ">="
        "ltWord#"           -> cmpOp cunit "<"
        "leWord#"           -> cmpOp cunit "<="


        "newPinnedByteArray#" -> alloc (cunit <+> ppRegister a) <> semi
        "newAlignedPinnedByteArray#" -> alloc (cunit <+> ppRegister a) <> semi
        "unsafeFreezeByteArray#" -> ppRegister a <> semi
        "unsafeFreezeArray#" -> ppRegister a <> semi
        "byteArrayContents#" -> ppRegister a <> semi
        "ord#"              -> ppRegister a <> semi
        "chr#"              -> ppRegister a <> semi
        
        "narrow8Word#"      -> unOp cu8 ""
        "narrow16Word#"     -> unOp cu16 ""
        "narrow32Word#"     -> unOp cu32 ""
        "narrow8Int#"       -> unOp cs8 ""
        "narrow16Int#"      -> unOp cs16 ""
        "narrow32Int#"      -> unOp cs32 ""

        "newMutVar"         -> text "newMutVar" <> parens (ppRegister a) <> semi
        "readMutVar"        -> text "readMutVar" <> parens (ppRegister a) <> semi

        "uncheckedShiftL#"  -> binOp' cunit cs32 "<<"
        "uncheckedShiftR#"  -> binOp' cunit cs32 ">>"
        "uncheckedIShiftL#"  -> binOp' csunit cs32 "<<"
        "uncheckedIShiftR#"  -> binOp' csunit cs32 ">>"

        "mkWeak#"           -> int 0 <> semi

        "readWord64OffAddr#" -> indexAnyArray cu64p a b <> semi

        _other              -> int 0 <> semi <+> panic ("Unhandled set primop: " ++ show prim)
    where indexAnyArray ty arr idx
              = parens (ty <+> parens (ppRegister arr)) <> brackets (cunit <+> ppRegister idx)
          binOp ty fn       =  parens (ty <+> ppRegister a <+> text fn <+> ty <+> ppRegister b) <> semi
          binOpCast cast fn = castToWord (cast a <+> text fn <+> cast b) <> semi
          binOp' ty ty' fn  = parens (ty <+> ppRegister a <+> text fn <+> ty' <+> ppRegister b) <> semi
          unOp ty fn        = parens (text fn <+> parens (ty <+> ppRegister a)) <> semi
          cmpOp ty fn       = ifStatement (ty <> ppRegister a <+> text fn <+> ty <> ppRegister b)
                                (int 1)
                                (int 0)
          cmpDoubleOp fn    = ifStatement (castToDouble a <+> text fn <+> castToDouble b)
                                   (int 1)
                                   (int 0)
          --cmpFloatOp fn     = ifStatement (castToFloat a <+> text fn <+> castToFloat b)
          --                         (int 1)
          --                         (int 0)


ppValue :: Value -> Doc
ppValue value
    = case value of
        Node tag nt missingArity -> int (uniqueId tag) <+> comment (show tag)
        Lit (Lint i)          -> int (fromIntegral i)
        Lit (Lchar c)         -> int (ord c) <+> comment (show c)
        Lit (Lrational r)     -> castToWord (double (fromRational r))



castToWord double
    = text "doubleToWord" <> parens double
castToDouble ptr
    = text "wordToDouble" <> parens (ppRenamed ptr)



call name = ppRenamed name <> parens empty <> semi

alloc :: Doc -> Doc
alloc size = text "alloc" <> parens (size)

panic :: String -> Doc
panic txt = text "panic" <> parens (escString txt) <> semi <+> comment txt

escString :: String -> Doc
escString string = char '"' <> text (concatMap worker string) <> char '"'
    where worker c | False = [c]
                   | otherwise = printf "\\x%02x" (ord c)

ppRenamed :: Renamed -> Doc
ppRenamed (Anonymous i)
    = text "anon_" <> int i
ppRenamed (Aliased i name)
    = text "named_" <> sanitize name <> char '_' <> int i
ppRenamed (Builtin "undefined")
    = text "0"
ppRenamed (Builtin builtin)
    = error $ "Grin.Stage2.Backend.C.ppRenamed: Unknown primitive: " ++ show builtin

switch ::Doc -> Doc -> Doc
switch scrut body
    = text "switch" <> parens scrut <+> char '{' <$$>
      indent 2 body <$$>
      char '}'

ifStatement :: Doc -> Doc -> Doc -> Doc
ifStatement cond true false
    = parens cond <> text "?" <> parens true <> text ":" <> parens false <> semi


sanitize :: CompactString -> Doc
sanitize cs = text (map sanitizeChar $ show $ pretty cs)

sanitizeChar :: Char -> Char
sanitizeChar c | isAlphaNum c = c
               | otherwise    = '_'


comment :: String -> Doc
comment str = text "/*" <+> text (concatMap worker str) <+> text "*/"
    where worker '/' = "\\/"
          worker c = [c]



void     = text "void"
u8       = text "u8"
u16       = text "u16"
u32       = text "u32"
u64       = text "u64"
s8       = text "s8"
s16       = text "s16"
s32       = text "s32"
u64p     = text "u64*"
cu64p     = parens u64p
unit     = text "unit"
unitp    = text "unit*"
sunit     = text "sunit"
sunitp    = text "sunit*"

cunit  = parens unit
cunitp = parens unitp

csunit = parens sunit

cu8 = parens u8
cu16 = parens u16
cu32 = parens u32

cs8 = parens s8
cs16 = parens s16
cs32 = parens s32
cu8p = parens (u8<>char '*')


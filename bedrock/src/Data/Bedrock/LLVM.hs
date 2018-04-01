{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Bedrock.LLVM ( compile ) where

import           Control.Monad.Reader
import qualified Data.Map                           as Map
import qualified Data.Set                           as Set

import           Control.Monad.Except
import           Data.Bedrock                       as Bedrock
import           Data.Bedrock.GlobalVariables       (allRegisters)
import           Data.Bedrock.LLVMGen
import           Data.List
import           Data.Word
import qualified Data.Text.Lazy.IO as T
import           System.FilePath

import           LLVM.AST as LLVM
import           LLVM.AST.DataLayout as LLVM
import           LLVM.AST.AddrSpace as LLVM
import           LLVM.AST.Name as LLVM
import           LLVM.AST.Global as Global
import           LLVM.AST.Linkage as LLVM
import qualified LLVM.AST.Constant as Constant
import           LLVM.AST.Visibility
import           LLVM.AST.CallingConvention
import           LLVM.AST.Attribute
import           LLVM.Pretty as LLVM
import Data.String

compile :: Bedrock.Module -> FilePath -> IO ()
compile bedrock path =
  T.writeFile path (ppllvm (toLLVM bedrock))

toLLVM :: Bedrock.Module -> LLVM.Module
toLLVM bedrock = LLVM.Module
    { moduleName = "main"
    , moduleSourceFileName = "blank.hs"
    , moduleDataLayout = Just dataLayout
    , moduleTargetTriple = Nothing
    , moduleDefinitions =
        [ GlobalDefinition functionDefaults
            { name = LLVM.Name (fromString foreignName)
            , returnType = cTypeToLLVM foreignReturn
            , parameters = ([ Parameter (cTypeToLLVM ty) (UnName 0) []
                            | ty <- foreignArguments], False) }
        | Foreign{..} <- modForeigns bedrock ] ++
        [ GlobalDefinition globalVariableDefaults
            { name = LLVM.Name (fromString $ "bedrock:"++reg)
            , linkage = LLVM.Private
            , visibility = Default
            , threadLocalMode = Nothing
            , addrSpace = AddrSpace 0
            , unnamedAddr = Just GlobalAddr
            , isConstant = False
            , Global.type' = PointerType (IntegerType 64) (AddrSpace 0)
            , initializer = Just $ Constant.Null $ PointerType (IntegerType 64) (AddrSpace 0)
            , section = Nothing }
        | reg <- Set.toList $ allRegisters bedrock ] ++
        defs
    }
  where
    dataLayout = defaultDataLayout LittleEndian
    defs = execGenModule (mkEnv bedrock) $ do
            newDefinition $ GlobalDefinition functionDefaults
                    { name = LLVM.Name "exit"
                    , returnType = VoidType
                    , parameters = ([ Parameter (IntegerType 32) (UnName 0) []], False)
                    }
            newDefinition $ GlobalDefinition functionDefaults
                    { name = LLVM.Name "llvm.trap"
                    , returnType = VoidType
                    , parameters = ([], False)
                    }
            newDefinition $ GlobalDefinition functionDefaults
                    { name = LLVM.Name "puts"
                    , returnType = IntegerType 32
                    , parameters = ([ Parameter (PointerType (IntegerType 8) (AddrSpace 0)) (UnName 0) []], False)
                    }

            mainDef
            forM_ (functions bedrock) $ \Bedrock.Function{..} -> do
                blocks <- genBlocks $ blockToLLVM fnBody
                newDefinition $ GlobalDefinition functionDefaults
                    { name = nameToLLVM fnName
                    , returnType = typesToLLVM fnResults
                    , parameters = ([ Parameter
                                        (typeToLLVM variableType)
                                        (nameToLLVM variableName)
                                        []
                                    | Variable{..} <- fnArguments ], False)
                    , visibility = Default
                    , linkage = LLVM.Internal
                    , Global.callingConvention = Fast
                    , basicBlocks = blocks
                    , Global.functionAttributes = [Right NoUnwind]
                    }
    mainDef = do
        let wordPtrTy = PointerType (IntegerType 64) (AddrSpace 0)
            entryCall = Call
                { tailCallKind = Nothing
                , callingConvention = Fast
                , returnAttributes = []
                , function = Right $ ConstantOperand $ Constant.GlobalReference
                                (FunctionType VoidType [] False)
                                (nameToLLVM $ entryPoint bedrock)
                , arguments =
                    [ (ConstantOperand $ Constant.Null wordPtrTy, [])
                    , (ConstantOperand $ Constant.Null wordPtrTy, []) ]
                , functionAttributes = [Right NoUnwind, Right NoReturn]
                , metadata = [] }
        newDefinition $ GlobalDefinition functionDefaults
            { name = LLVM.Name "main"
            , returnType = IntegerType 32
            , basicBlocks = [BasicBlock (UnName 0) [Do entryCall] (Do $ Unreachable [])] }


mkEnv :: Bedrock.Module -> Env
mkEnv bedrock = env
  where
    env = Env
        { envNodeMapping = Map.fromList (zip allNodes [0..])
        , envFunctionTypes = Map.fromList fnTypes }
    allNodes =
        [ ConstructorName name blanks
        | NodeDefinition name args <- nodes bedrock
        , blanks <- [0..length args] ] ++
        [ FunctionName fnName blanks
        | Bedrock.Function{..} <- functions bedrock
        , blanks <- [0..length fnArguments] ]
    fnTypes =
      [ (fnName, FunctionType (typesToLLVM fnResults)
                      [ typeToLLVM variableType
                      | Variable{..} <- fnArguments ] False)
      | Bedrock.Function{..} <- functions bedrock ]
{-

toLLVM :: Bedrock.Module -> LLVM.Module
toLLVM bedrock = defaultModule
    { moduleName = "Bedrock"
    , moduleDataLayout = Just dataLayout
    , moduleDefinitions =
        [ GlobalDefinition functionDefaults
            { name = LLVM.Name foreignName
            , returnType = cTypeToLLVM foreignReturn
            , parameters = ([ Parameter (cTypeToLLVM ty) (UnName 0) []
                            | ty <- foreignArguments], False) }
        | Foreign{..} <- modForeigns bedrock ] ++
        [ GlobalDefinition globalVariableDefaults
            { name = LLVM.Name ("bedrock:"++reg)
            , linkage = LLVM.Private
            , visibility = Default
            , threadLocalMode = Nothing
            , addrSpace = AddrSpace 0
            , hasUnnamedAddr = True
            , isConstant = False
            , Global.type' = PointerType (IntegerType 64) (AddrSpace 0)
            , initializer = Just $ Constant.Null $ PointerType (IntegerType 64) (AddrSpace 0)
            , section = Nothing }
        | reg <- Set.toList $ allRegisters bedrock ] ++
        defs
    }
  where
    dataLayout = defaultDataLayout LittleEndian
    defs = execGenModule (mkEnv bedrock) $ do
            newDefinition $ GlobalDefinition functionDefaults
                    { name = LLVM.Name "exit"
                    , returnType = VoidType
                    , parameters = ([ Parameter (IntegerType 32) (UnName 0) []], False)
                    }
            newDefinition $ GlobalDefinition functionDefaults
                    { name = LLVM.Name "llvm.trap"
                    , returnType = VoidType
                    , parameters = ([], False)
                    }
            newDefinition $ GlobalDefinition functionDefaults
                    { name = LLVM.Name "puts"
                    , returnType = IntegerType 32
                    , parameters = ([ Parameter (PointerType (IntegerType 8) (AddrSpace 0)) (UnName 0) []], False)
                    }

            mainDef
            forM_ (functions bedrock) $ \Bedrock.Function{..} -> do
                blocks <- genBlocks $ blockToLLVM fnBody
                newDefinition $ GlobalDefinition functionDefaults
                    { name = nameToLLVM fnName
                    , returnType = typesToLLVM fnResults
                    , parameters = ([ Parameter
                                        (typeToLLVM variableType)
                                        (nameToLLVM variableName)
                                        []
                                    | Variable{..} <- fnArguments ], False)
                    , visibility = Default
                    , linkage = LLVM.Internal
                    , Global.callingConvention = Fast
                    , basicBlocks = blocks
                    , Global.functionAttributes = [Right NoUnwind]
                    }
    mainDef = do
        let wordPtrTy = PointerType (IntegerType 64) (AddrSpace 0)
            entryCall = Call
                { tailCallKind = Nothing
                , callingConvention = Fast
                , returnAttributes = []
                , function = Right $ ConstantOperand $ Constant.GlobalReference
                                VoidType
                                (nameToLLVM $ entryPoint bedrock)
                , arguments =
                    [ (ConstantOperand $ Constant.Null wordPtrTy, [])
                    , (ConstantOperand $ Constant.Null wordPtrTy, []) ]
                , functionAttributes = [Right NoUnwind, Right NoReturn]
                , metadata = [] }
        newDefinition $ GlobalDefinition functionDefaults
            { name = LLVM.Name "main"
            , returnType = IntegerType 32
            , basicBlocks = [BasicBlock (UnName 0) [Do entryCall] (Do $ Unreachable [])] }

runE :: ExceptT String IO a -> IO a
runE action = do
    ret <- runExceptT action
    case ret of
        Left msg  -> error msg
        Right val -> return val

compile :: Bedrock.Module -> FilePath -> IO ()
compile bedrock dst =
    withContext $ \ctx -> do
    let m = toLLVM bedrock
    writeFile (replaceExtension dst "pretty") (showPretty m)
    runE $ withModuleFromAST ctx (toLLVM bedrock) $ \llvmModule -> do
        runE (writeLLVMAssemblyToFile (File dst) llvmModule)
    return ()


-}
cTypeToLLVM :: CType -> LLVM.Type
cTypeToLLVM cType =
    case cType of
        I8 -> IntegerType 8
        I32 -> IntegerType 32
        I64 -> IntegerType 64
        IWord -> IntegerType 64
        CPointer subTy -> PointerType (cTypeToLLVM subTy) (AddrSpace 0)
        CVoid -> VoidType
        CFunction retTy argTys ->
            FunctionType (cTypeToLLVM retTy) (map cTypeToLLVM argTys) False

typesToLLVM :: [Bedrock.Type] -> LLVM.Type
typesToLLVM [] = VoidType
typesToLLVM [ty] = typeToLLVM ty
typesToLLVM lst = StructureType False (map typeToLLVM lst)

typeToLLVM :: Bedrock.Type -> LLVM.Type
typeToLLVM ty =
    case ty of
        NodePtr -> PointerType (IntegerType 64) (AddrSpace 0)
        Node -> IntegerType 64
        StaticNode{} -> IntegerType 64
        Primitive cType -> cTypeToLLVM cType
        FramePtr -> PointerType (IntegerType 64) (AddrSpace 0)

bitSize :: LLVM.Type -> Word32
bitSize (IntegerType n) = n
bitSize _ = error "Data.Bedrock.LLVM.bitSize"

nameToLLVM :: Bedrock.Name -> LLVM.Name
nameToLLVM name | nameUnique name == 0 =
  LLVM.Name $ fromString $ intercalate "." (nameModule name ++ [nameIdentifier name])
nameToLLVM name = LLVM.Name $ fromString $ concat
    [ intercalate "." (nameModule name ++ [nameIdentifier name])
    , "_"
    , show (nameUnique name) ]

traceLLVM :: String -> GenBlocks ()
traceLLVM msg = do
    strGlobal <- globalString msg
    str <- anonInst GetElementPtr
        { inBounds = True
        , address = ConstantOperand $ Constant.GlobalReference
                        (PointerType
                            (ArrayType (fromIntegral (length msg)+1) (IntegerType 8))
                            (AddrSpace 0))
                        strGlobal
        , indices = [ConstantOperand $ Constant.Int 64 0, ConstantOperand $ Constant.Int 64 0]
        , metadata = [] }
    anonInst $ Call
        { tailCallKind = Nothing
        , callingConvention = C
        , returnAttributes = []
        , function = Right $ ConstantOperand $ Constant.GlobalReference
                        (FunctionType (IntegerType 32) [PointerType (IntegerType 8) (AddrSpace 0)] False)
                        (LLVM.Name "puts")
        , arguments =
            [ (LocalReference (PointerType (IntegerType 8) (AddrSpace 0)) str, []) ]
        , functionAttributes = []
        , metadata = [] }
    return ()

patternTag :: Pattern -> String
patternTag pat =
  case pat of
    NodePat nodeName [] ->
      case nodeName of
        ConstructorName name missing -> ppPartial name missing
        FunctionName name missing -> ppPartial name missing
        UnboxedTupleName -> "(# #)"
    LitPat (LiteralInt i) -> "literal " ++ show i
    _ -> ""
  where
    ppPartial name missing =
      unwords (ppName name : replicate missing "_")
    ppName (Bedrock.Name ns ident unique) =
      case unique of
        0 -> intercalate "." (ns++[ident])
        _ -> intercalate "." (ns++[ident]) ++ "_" ++ show unique

blockToLLVM :: Block -> GenBlocks Terminator
blockToLLVM = worker
  where
    worker block = do
      case block of
        Case Variable{..} mbDefault alts -> do
          branches <- forM alts $ \(Alternative pattern branch) -> do
            branchName <- newTaggedBlock (patternTag pattern) $ do
                            -- case pattern of
                            --   NodePat nodeName [] ->
                            --     traceLLVM $ "Branch taken: " ++ show nodeName
                            --   LitPat (LiteralInt i) ->
                            --     traceLLVM $ "Branch taken: Lit: " ++ show i
                            --   _ -> return ()
                            worker branch
            case pattern of
              NodePat nodeName [] -> do
                ident <- resolveNodeName nodeName
                return (Constant.Int 64 ident, branchName)
              NodePat{} ->
                error "Data.Bedrock.LLVM: Invalid input. Arguments must be lowered."
              LitPat (LiteralInt i) -> do
                return ( Constant.Int (bitSize $ typeToLLVM variableType) i
                       , branchName)
              LitPat LiteralString{} ->
                error "Data.Bedrock.LLVM: Case over unboxed strings not supported."
          defaultName <- newBlock $
            case mbDefault of
              Nothing -> do
                traceLLVM $ "Unreachable branch :("
                doInst $ Call Nothing C [] (Right $ ConstantOperand $ Constant.GlobalReference
                        (FunctionType VoidType [] False)
                        (LLVM.Name "llvm.trap")) [] [] []
                return $ Ret Nothing []
              Just branch -> worker branch
          return $ Switch
              { operand0' = LocalReference
                                  (typeToLLVM variableType)
                                  (nameToLLVM variableName)
              , defaultDest = defaultName
              , dests = branches
              , metadata' = [] }
        Bind [Variable{..}] expr next -> do
            let name = nameToLLVM variableName
            inst <- mkInst (typeToLLVM variableType) expr
            pushInst (name := inst)
            worker next
        Bind [] expr next -> do
            doInst =<< mkInst VoidType expr
            worker next
        TailCall fName args -> do
            -- traceLLVM $ "TailCall: " ++ show fName
            doInst $ Call
                { tailCallKind = Just Tail
                , callingConvention = Fast
                , returnAttributes = []
                , function = Right $ ConstantOperand $ Constant.GlobalReference
                                (FunctionType VoidType [] False)
                                (nameToLLVM fName)
                , arguments =
                    [ (LocalReference
                        (typeToLLVM variableType)
                        (nameToLLVM variableName), [])
                    | Variable{..} <- args ]
                , functionAttributes = [Right NoUnwind]
                , metadata = [] }
            return $ Ret Nothing []
        Exit -> do
            doInst $ Call
                { tailCallKind = Nothing
                , callingConvention = C
                , returnAttributes = []
                , function = Right $ ConstantOperand $ Constant.GlobalReference
                                (FunctionType VoidType [] False)
                                (LLVM.Name "exit")
                , arguments = [ (ConstantOperand $ Constant.Int 32 0, []) ]
                , functionAttributes = []
                , metadata = [] }
            return $ Unreachable []
        Return [] -> return $ Ret Nothing []
        Bedrock.Invoke cont args -> do
            -- traceLLVM $ "Bedrock: Invoke"
            fnPtr <- anonInst $ castReference
                        (typeToLLVM $ variableType cont)
                        (nameToLLVM $ variableName cont)
                        (PointerType (FunctionType VoidType
                            (map (typeToLLVM . variableType) args)
                            False) (AddrSpace 0))
            doInst $ Call
                { tailCallKind = Just Tail
                , callingConvention = Fast
                , returnAttributes = []
                , function = Right $ LocalReference
                                (FunctionType VoidType [] False)
                                fnPtr
                , arguments =
                    [ (LocalReference
                        (typeToLLVM variableType)
                        (nameToLLVM variableName), [])
                    | Variable{..} <- args ]
                , functionAttributes = [Right NoUnwind]
                , metadata = [] }
            return $ Unreachable []
        _ -> do
            traceLLVM $ "Bedrock: Internal error: Unhandled construct"
            doInst $ Call
                { tailCallKind = Nothing
                , callingConvention = C
                , returnAttributes = []
                , function = Right $ ConstantOperand $ Constant.GlobalReference
                                (FunctionType VoidType [] False)
                                (LLVM.Name "exit")
                , arguments = [ (ConstantOperand $ Constant.Int 32 1, []) ]
                , functionAttributes = []
                , metadata = [] }
            return $ Unreachable []

    -- mkInst retTy expr | trace ("Expr: " ++ show expr) False = undefined

    -- Prim-ops
    mkInst retTy (CCall "indexI8#" [Variable{..}]) =
        return LLVM.Load
            { volatile = False
            , address = LocalReference
                            (typeToLLVM variableType)
                            (nameToLLVM variableName)
            , maybeAtomicity = Nothing
            , alignment = 1
            , metadata = [] }
    mkInst retTy (CCall "cast" [Variable{..}]) = return $
        castReference (typeToLLVM variableType) (nameToLLVM variableName) retTy
    mkInst retTy (CCall "addrAdd#" [ptr, offset]) =
        return $ GetElementPtr
            { inBounds = True
            , address = LocalReference
                            (typeToLLVM $ variableType ptr)
                            (nameToLLVM $ variableName ptr)
            , indices = [LocalReference
                            (typeToLLVM $ variableType offset)
                            (nameToLLVM $ variableName offset)]
            , metadata = []
            }

    mkInst retTy (CCall fName args) = do
        return Call
            { tailCallKind = Nothing
            , callingConvention = C
            , returnAttributes = []
            , function = Right (ConstantOperand $ Constant.GlobalReference
                                    (FunctionType retTy [] False)
                                    (LLVM.Name $ fromString fName))
            , arguments =
                [ (LocalReference (typeToLLVM variableType) (nameToLLVM variableName), [])
                | Variable{..} <- args ]
            , functionAttributes = []
            , metadata = [] }
    mkInst retTy (Application fName args) = do
        return Call
            { tailCallKind = Nothing
            , callingConvention = Fast
            , returnAttributes = []
            , function = Right (ConstantOperand $ Constant.GlobalReference
                                    (FunctionType retTy [] False)
                                    (nameToLLVM fName))
            , arguments =
                [ (LocalReference (typeToLLVM variableType) (nameToLLVM variableName), [])
                | Variable{..} <- args ]
            , functionAttributes = []
            , metadata = [] }
    mkInst retTy Undefined = return BitCast
        { operand0 = ConstantOperand $ Constant.Undef retTy
        , type' = retTy
        , metadata = [] }
    mkInst retTy (WriteGlobal reg Variable{..}) = return LLVM.Store
            { volatile = False
            , address = ConstantOperand $ Constant.GlobalReference
                            (PointerType (typeToLLVM variableType) (AddrSpace 0))
                            (LLVM.Name $ fromString $ "bedrock:" ++ reg)
            , value = LocalReference (typeToLLVM variableType) (nameToLLVM variableName)
            , maybeAtomicity = Nothing
            , alignment = 1
            , metadata = []
            }
    mkInst retTy (ReadGlobal reg) = return LLVM.Load
            { volatile = False
            , address = ConstantOperand $ Constant.GlobalReference
                            (PointerType retTy (AddrSpace 0))
                            (LLVM.Name $ fromString $ "bedrock:" ++ reg)
            , maybeAtomicity = Nothing
            , alignment = 1
            , metadata = [] }
    mkInst retTy (Bedrock.Load attrs Variable{..} offset) = do
        offsetPtr <- anonInst $ GetElementPtr
            { inBounds = True
            , address = LocalReference
                            (typeToLLVM variableType)
                            (nameToLLVM variableName)
            , indices = [ConstantOperand $ Constant.Int 64 (fromIntegral offset)]
            , metadata = []
            }
        word <- anonInst $ LLVM.Load
            { volatile = False
            , address = LocalReference
                            (typeToLLVM variableType)
                            offsetPtr
            , maybeAtomicity = Nothing
            , alignment = 1
            , metadata =
                if memConstant attrs
                    then [("invariant.load", MetadataNode [])]
                    else [] }
        return $ castReference (IntegerType 64) word retTy
    mkInst retTy (Write dst offset var) = do
        casted <- anonInst $ castReference
                    (typeToLLVM $ variableType var)
                    (nameToLLVM $ variableName var)
                    (IntegerType 64)
        offsetPtr <- anonInst $ GetElementPtr
            { inBounds = True
            , address = LocalReference
                            (typeToLLVM $ variableType dst)
                            (nameToLLVM $ variableName dst)
            , indices = [ConstantOperand $ Constant.Int 64 (fromIntegral offset)]
            , metadata = [] }
        return $ LLVM.Store
            { volatile = False
            , address = LocalReference
                            (typeToLLVM (variableType dst))
                            offsetPtr
            , value = LocalReference
                            (IntegerType 64)
                            casted
            , maybeAtomicity = Nothing
            , alignment = 1
            , metadata = [] }
    mkInst retTy (Address ptr offset) =
        return $ GetElementPtr
            { inBounds = True
            , address = LocalReference
                            (typeToLLVM $ variableType ptr)
                            (nameToLLVM $ variableName ptr)
            , indices = [ConstantOperand $ Constant.Int 64 (fromIntegral offset)]
            , metadata = [] }
    mkInst retTy (FunctionPointer fnName) = do
      fnTy <- getFunctionType fnName
      return BitCast
          { operand0 = ConstantOperand $ Constant.GlobalReference (PointerType fnTy (AddrSpace 0)) (nameToLLVM fnName)
          , type' = retTy
          , metadata = [] }
    mkInst retTy (MkNode nodeName []) = do
        ident <- resolveNodeName nodeName
        return BitCast
            { operand0 = ConstantOperand $ Constant.Int 64 ident
            , type' = retTy
            , metadata = [] }
    mkInst retTy (Literal (LiteralInt i)) = return BitCast
        { operand0 = ConstantOperand $ Constant.Int (bitSize retTy) i
        , type' = retTy
        , metadata = [] }
    mkInst _retTy (Literal (LiteralString str)) = do
        strGlobal <- globalString str
        return GetElementPtr
            { inBounds = True
            , address = ConstantOperand $ Constant.GlobalReference
                            (PointerType
                                (ArrayType (fromIntegral (length str)+1) (IntegerType 8))
                                (AddrSpace 0))
                            strGlobal
            , indices =
                [ ConstantOperand $ Constant.Int 64 0
                , ConstantOperand $ Constant.Int 64 0]
            , metadata = [] }

    mkInst retTy (TypeCast Variable{..}) = return $
        castReference (typeToLLVM variableType) (nameToLLVM variableName) retTy

    mkInst retTy expr = error $ "Data.Bedrock.LLVM: Unhandled expression: " ++ show expr
        -- return BitCast
        -- { operand0 = ConstantOperand $ Constant.Undef retTy
        -- , type' = retTy
        -- , metadata = [] }

castReference :: LLVM.Type -> LLVM.Name -> LLVM.Type -> Instruction
castReference origType origName destType =
    (cons origType destType)
        (LocalReference origType origName)
        destType
        []
  where
    cons PointerType{} IntegerType{}             = PtrToInt
    cons IntegerType{} PointerType{}             = IntToPtr
    cons (IntegerType a) (IntegerType b) | a > b = Trunc
    cons (IntegerType a) (IntegerType b) | a < b = ZExt
    cons _ _                                     = BitCast

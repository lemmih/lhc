{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Bedrock.LLVM ( compile ) where

import           Control.Monad.Reader
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set

import           Data.Bedrock                 as Bedrock
import           Data.Bedrock.GlobalVariables (allRegisters)
import           Data.Bedrock.LLVMGen
import Data.Bedrock.PrettyPrint (pretty)
import           Data.Char
import           Data.List
import qualified Data.Text.Lazy.IO            as T
import           Data.Word

import           Data.String
import           LLVM.AST                     as LLVM
import           LLVM.AST.AddrSpace           as LLVM
import           LLVM.AST.Attribute
import           LLVM.AST.CallingConvention
import qualified LLVM.AST.Constant            as Constant
import           LLVM.AST.Global              as Global
import           LLVM.AST.Linkage             as LLVM
import           LLVM.AST.Type                (i32, i64, i8)
import qualified LLVM.AST.Type                as LLVM
import           LLVM.AST.Visibility
import           LLVM.Pretty                  as LLVM

compile :: Bedrock.Module -> FilePath -> IO ()
compile bedrock path =
  T.writeFile path (ppllvm (toLLVM bedrock))

toLLVM :: Bedrock.Module -> LLVM.Module
toLLVM bedrock = LLVM.Module
    { moduleName = "main"
    , moduleSourceFileName = "blank.hs"
    , moduleDataLayout = Nothing
    , moduleTargetTriple = Nothing
    , moduleDefinitions =
        TypeDefinition (mkName "word") (Just i64) :
        [ GlobalDefinition functionDefaults
            { name = LLVM.Name (fromString foreignName)
            , returnType = foreignReturn
            , parameters = ([ Parameter ty (UnName 0) []
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
            , Global.type' = LLVM.ptr wordTy
            , initializer = Just $ Constant.Null $ LLVM.ptr wordTy
            , section = Nothing }
        | reg <- Set.toList $ allRegisters bedrock ] ++
        defs
    }
  where
    defs = execGenModule (mkEnv bedrock) $ do
      newDefinition $ GlobalDefinition functionDefaults
              { name = LLVM.Name "exit"
              , returnType = VoidType
              , parameters = ([ Parameter i32 (UnName 0) []], False)
              }
      newDefinition $ GlobalDefinition functionDefaults
              { name = LLVM.Name "llvm.trap"
              , returnType = VoidType
              , parameters = ([], False)
              }
      newDefinition $ GlobalDefinition functionDefaults
              { name = LLVM.Name "puts"
              , returnType = i32
              , parameters = ([ Parameter (LLVM.ptr i8) (UnName 0) []], False)
              }
      newDefinition $ GlobalDefinition functionDefaults
              { name = LLVM.Name "_lhc_mkTag"
              , returnType = wordTy
              , parameters = ([ Parameter wordTy (UnName 0) []], False)
              }
      newDefinition $ GlobalDefinition functionDefaults
              { name = LLVM.Name "_lhc_getTag"
              , returnType = wordTy
              , parameters = ([ Parameter wordTy (UnName 0) []], False)
              }
      newDefinition $ GlobalDefinition functionDefaults
              { name = LLVM.Name "_lhc_isIndirectionP"
              , returnType = wordTy
              , parameters = ([ Parameter (LLVM.ptr wordTy) (UnName 0) []], False)
              }
      newDefinition $ GlobalDefinition functionDefaults
              { name = LLVM.Name "_lhc_getIndirectionP"
              , returnType = LLVM.ptr wordTy
              , parameters = ([ Parameter (LLVM.ptr wordTy) (UnName 0) []], False)
              }
      newDefinition $ GlobalDefinition functionDefaults
              { name = LLVM.Name "_lhc_setIndirection"
              , returnType = VoidType
              , parameters = ([ Parameter (LLVM.ptr wordTy) (UnName 0) []
                              , Parameter (LLVM.ptr wordTy) (UnName 1) []], False)
              }
      newDefinition $ GlobalDefinition functionDefaults
              { name = LLVM.Name "_lhc_loadLast"
              , returnType = LLVM.ptr wordTy
              , parameters = ([ Parameter (LLVM.ptr wordTy) (UnName 0) []
                              , Parameter wordTy (UnName 1) []
                              , Parameter wordTy (UnName 2) []], False)
              }
      getLayoutDef

      mainDef
      forM_ (functions bedrock) $ \Bedrock.Function{..} -> do
        blocks <- genBlocks $ blockToLLVM fnBody
        mbPrefix <- attributesToPrefix fnAttributes
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
            , Global.callingConvention = lhcCC
            , basicBlocks = blocks
            , prefix = mbPrefix
            , Global.functionAttributes = [Right NoUnwind]
            }
    mainDef = do
      let wordPtrTy = LLVM.ptr wordTy
          entryCall = Call
              { tailCallKind = Nothing
              , callingConvention = lhcCC
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
        { name = LLVM.Name "_lhc_main"
        , returnType = i32
        , basicBlocks = [BasicBlock (UnName 0) [Do entryCall] (Do $ Unreachable [])] }
    getLayoutDef = do
      layouts <- asks envNodeLayout
      let nChars :: Num a => a
          nChars = 128
          nLayouts :: Num a => a
          nLayouts = fromIntegral (length layouts)
          infoTableType = StructureType False [i32, i32, LLVM.ArrayType nChars i8]
          infoTablesType = ArrayType nLayouts infoTableType
      newDefinition $ GlobalDefinition globalVariableDefaults
        { name = LLVM.Name "_lhc_info_tables"
        , isConstant = True
        , Global.type' = infoTablesType
        , initializer = Just $ Constant.Array infoTableType
            [ Constant.Struct Nothing False
                [ Constant.Int 32 (fromIntegral prim)
                , Constant.Int 32 (fromIntegral ptrs)
                , Constant.Array i8 [Constant.Int 8 (fromIntegral $ ord c) | c <- str]
                ]
            | (node, (prim, ptrs)) <- layouts
            , let str = take nChars (show (pretty node) ++ repeat '\0') ]
        }
      newDefinition $ GlobalDefinition globalVariableDefaults
        { name = LLVM.Name "_lhc_info_table_size"
        , isConstant = True
        , Global.type' = i32
        , initializer = Just $ Constant.Int 32 nLayouts
        }

attributesToPrefix :: [Bedrock.Attribute] -> GenModule (Maybe Constant.Constant)
attributesToPrefix = worker []
  where
    worker [] [] = return Nothing
    worker [single] [] = return (Just single)
    worker acc [] = return (Just $ Constant.Vector acc)
    worker _acc (Prefix size prims ptrs _handler : _xs) =
      return $ Just $ Constant.Struct Nothing False
        [Constant.Int 32 (fromIntegral size)
        ,Constant.Int 32 (fromIntegral prims)
        ,Constant.Int 32 (fromIntegral ptrs) ]
    worker acc (_:xs) = worker acc xs

mkEnv :: Bedrock.Module -> Env
mkEnv bedrock = env
  where
    env = Env
        { envNodeMapping = Map.fromList (zip allNodes [0..])
        , envNodeLayout = nodeLayouts
        , envFunctionTypes = Map.fromList fnTypes }
    allNodes =
        [ name
        | NodeLayout name _prim _ptrs <- modLayouts bedrock ]
        -- [ ConstructorName name blanks
        -- | NodeDefinition name args <- nodes bedrock
        -- , blanks <- [0..length args] ] ++
        -- [ FunctionName fnName blanks
        -- | Bedrock.Function{..} <- functions bedrock
        -- , blanks <- [0..length fnArguments] ]
    nodeLayouts =
        [ (name, (prim, ptrs))
        | NodeLayout name prim ptrs <- modLayouts bedrock ]
        -- [ (ConstructorName name blanks, computeNodeLayout (drop blanks $ reverse args))
        -- | NodeDefinition name args <- nodes bedrock
        -- , blanks <- [0..length args] ] ++
        -- [ (FunctionName fnName blanks, computeNodeLayout (drop blanks $ reverse $ map variableType $ filter (not.isHP) fnArguments))
        -- | Bedrock.Function{..} <- functions bedrock
        -- , blanks <- [0..length fnArguments] ]
    fnTypes =
      [ (fnName, FunctionType (typesToLLVM fnResults)
                      [ typeToLLVM variableType
                      | Variable{..} <- fnArguments ] False)
      | Bedrock.Function{..} <- functions bedrock ]

lhcCC :: CallingConvention
lhcCC = C

typesToLLVM :: [Bedrock.Type] -> LLVM.Type
typesToLLVM []   = VoidType
typesToLLVM [ty] = typeToLLVM ty
typesToLLVM lst  = StructureType False (map typeToLLVM lst)

typeToLLVM :: Bedrock.Type -> LLVM.Type
typeToLLVM ty =
    case ty of
        NodePtr         -> LLVM.ptr wordTy
        Node            -> wordTy
        StaticNode{}    -> wordTy
        IWord           -> wordTy
        Primitive cType -> cType
        FramePtr{}      -> LLVM.ptr wordTy

bitSize :: LLVM.Type -> Word32
bitSize (IntegerType n) = n
bitSize (NamedTypeReference (LLVM.Name "word")) = 64
bitSize ty              = error $ "Data.Bedrock.LLVM.bitSize: " ++ show ty

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
                            (ArrayType (fromIntegral (length msg)+1) i8)
                            (AddrSpace 0))
                        strGlobal
        , indices = [ConstantOperand $ Constant.Int 64 0, ConstantOperand $ Constant.Int 64 0]
        , metadata = [] }
    anonInst $ Call
        { tailCallKind = Nothing
        , callingConvention = C
        , returnAttributes = []
        , function = Right $ ConstantOperand $ Constant.GlobalReference
                        (FunctionType i32 [LLVM.ptr i8] False)
                        (LLVM.Name "puts")
        , arguments =
            [ (LocalReference (LLVM.ptr i8) str, []) ]
        , functionAttributes = []
        , metadata = [] }
    return ()

nodeNameTag :: NodeName -> String
nodeNameTag nodeName =
    case nodeName of
      ConstructorName name missing -> ppPartial name missing
      FunctionName name missing    -> ppPartial name missing
      UnboxedTupleName             -> "(# #)"
  where
    ppPartial name missing =
      unwords (ppName name : replicate missing "_")
    ppName (Bedrock.Name ns ident unique) =
      case unique of
        0 -> intercalate "." (ns++[ident])
        _ -> intercalate "." (ns++[ident]) ++ "_" ++ show unique

patternTag :: Pattern -> String
patternTag pat =
  case pat of
    NodePat nodeName []   -> nodeNameTag nodeName
    LitPat (LiteralInt i) -> "literal " ++ show i
    _                     -> ""

toLocalReference :: Variable -> Operand
toLocalReference var =
  LocalReference (typeToLLVM $ variableType var) (nameToLLVM $ variableName var)

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
                return $ Unreachable []
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
            fnTy@(FunctionType retTy _argTypes _isVarArg) <- getFunctionType fName
            if retTy == VoidType
              then do
                doInst $ Call
                    { tailCallKind = Just Tail
                    , callingConvention = lhcCC
                    , returnAttributes = []
                    , function = Right $ ConstantOperand $ Constant.GlobalReference
                                    fnTy
                                    (nameToLLVM fName)
                    , arguments =
                        [ (toLocalReference arg, [])
                        | arg <- args ]
                    , functionAttributes = [Right NoUnwind]
                    , metadata = [] }
                return $ Ret Nothing []
              else do
                ret <- anonInst $ Call
                    { tailCallKind = Just Tail
                    , callingConvention = lhcCC
                    , returnAttributes = []
                    , function = Right $ ConstantOperand $ Constant.GlobalReference
                                    fnTy
                                    (nameToLLVM fName)
                    , arguments =
                        [ (toLocalReference var, [])
                        | var <- args ]
                    , functionAttributes = [Right NoUnwind]
                    , metadata = [] }
                return $ Ret (Just $ LocalReference retTy ret) []
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
            return $ Ret Nothing []
        Return [] -> return $ Ret Nothing []
        Return [var] -> return $ Ret (Just $ toLocalReference var) []
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
                , callingConvention = lhcCC
                , returnAttributes = []
                , function = Right $ LocalReference
                                (FunctionType VoidType [] False)
                                fnPtr
                , arguments =
                    [ (toLocalReference arg, [])
                    | arg <- args ]
                , functionAttributes = [Right NoUnwind]
                , metadata = [] }
            return $ Ret Nothing []
        Panic str -> do
          traceLLVM $ "Bedrock: Panic: " ++ str
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
    mkInst _retTy (CCall "indexI8#" [addr]) =
        return LLVM.Load
            { volatile = False
            , address = toLocalReference addr
            , maybeAtomicity = Nothing
            , alignment = 1
            , metadata = [] }
    mkInst _retTy (CCall "addrAdd#" [ptr, offset]) =
        return $ GetElementPtr
            { inBounds = True
            , address = toLocalReference ptr
            , indices = [toLocalReference offset]
            , metadata = []
            }
    mkInst _retTy (CCall "-#" [a, b]) =
        return $ Sub
            { nsw = False
            , nuw = False
            , operand0 = toLocalReference a
            , operand1 = toLocalReference b
            , metadata = []
            }
    mkInst _retTy (CCall "+#" [a, b]) =
        return $ Add
            { nsw = False
            , nuw = False
            , operand0 = toLocalReference a
            , operand1 = toLocalReference b
            , metadata = []
            }
    mkInst _retTy (CCall "*#" [a, b]) =
        return $ Mul
            { nsw = False
            , nuw = False
            , operand0 = toLocalReference a
            , operand1 = toLocalReference b
            , metadata = []
            }
    mkInst _retTy (CCall "shl#" [a, b]) =
        return $ Shl
            { nsw = False
            , nuw = False
            , operand0 = toLocalReference a
            , operand1 = toLocalReference b
            , metadata = []
            }
    mkInst _retTy (CCall "sdiv#" [a, b]) =
        return $ SDiv
            { exact = False
            , operand0 = toLocalReference a
            , operand1 = toLocalReference b
            , metadata = []
            }
    mkInst _retTy (CCall "srem#" [a, b]) =
        return $ SRem
            { operand0 = toLocalReference a
            , operand1 = toLocalReference b
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
                [ (toLocalReference arg, [])
                | arg <- args ]
            , functionAttributes = []
            , metadata = [] }
    mkInst retTy (Application fName args) = do
        return Call
            { tailCallKind = Nothing
            , callingConvention = lhcCC
            , returnAttributes = []
            , function = Right (ConstantOperand $ Constant.GlobalReference
                                    (FunctionType retTy [] False)
                                    (nameToLLVM fName))
            , arguments =
                [ (toLocalReference arg, [])
                | arg <- args ]
            , functionAttributes = []
            , metadata = [] }
    mkInst retTy (InvokeReturn var args) = do
      -- traceLLVM $ "InvokeReturn"
      -- fnPtr <- anonInst $ castReference
      --             (typeToLLVM $ variableType cont)
      --             (nameToLLVM $ variableName cont)
      --             (PointerType (FunctionType VoidType
      --                 (map (typeToLLVM . variableType) args)
      --                 False) (AddrSpace 0))
      let fnTy = PointerType (FunctionType retTy
                  (map (typeToLLVM . variableType) args)
                  False) (AddrSpace 0)
      casted <- anonInst $ castReference
                  (typeToLLVM $ variableType var)
                  (nameToLLVM $ variableName var)
                  (PointerType fnTy (AddrSpace 0))
      fnOffset <- anonInst $ GetElementPtr
          { inBounds = True
          , address = LocalReference
                          (PointerType fnTy (AddrSpace 0))
                          casted
          , indices = [ConstantOperand $ Constant.Int 64 0]
          , metadata = [] }
      fnPtr <- anonInst $ LLVM.Load
              { volatile = False
              , address = LocalReference
                            (PointerType fnTy (AddrSpace 0))
                            fnOffset
              , maybeAtomicity = Nothing
              , alignment = 1
              , metadata = [] }
      return Call
          { tailCallKind = Nothing
          , callingConvention = lhcCC
          , returnAttributes = []
          , function = Right $ LocalReference
                          fnTy
                          fnPtr
          , arguments =
              [ (LocalReference
                  (typeToLLVM variableType)
                  (nameToLLVM variableName), [])
              | Variable{..} <- args ]
          , functionAttributes = [Right NoUnwind]
          , metadata = [] }
      -- cast var to fn ptr ptr
      -- peek at negative nth index
      -- call
    mkInst retTy Undefined = return BitCast
        { operand0 = ConstantOperand $ Constant.Undef retTy
        , type' = retTy
        , metadata = [] }
    mkInst _retTy (WriteGlobal reg Variable{..}) = return LLVM.Store
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
    mkInst retTy (Bedrock.Load Variable{..} offset) = do
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
            , metadata = [] }
        if variableType == NodePtr && offset == 0
          then return Call
              { tailCallKind = Nothing
              , callingConvention = C
              , returnAttributes = []
              , function = Right (ConstantOperand $ Constant.GlobalReference
                                      (FunctionType wordTy [wordTy] False)
                                      (LLVM.Name "_lhc_getTag"))
              , arguments =
                  [ (LocalReference wordTy word, []) ]
              , functionAttributes = []
              , metadata = [] }
          else return $ castReference wordTy word retTy
    mkInst retTy (Bedrock.LoadLastPtr ptr tag offset) = do
      return Call
          { tailCallKind = Nothing
          , callingConvention = C
          , returnAttributes = []
          , function = Right (ConstantOperand $ Constant.GlobalReference
                                  (FunctionType (LLVM.ptr wordTy) [LLVM.ptr wordTy, wordTy, wordTy] False)
                                  (LLVM.Name "_lhc_loadLast"))
          , arguments =
              [ (toLocalReference ptr, [])
              , (toLocalReference tag, [])
              , (ConstantOperand $ Constant.Int 64 (fromIntegral offset), []) ]
          , functionAttributes = []
          , metadata = [] }
    mkInst _retTy (Write dst offset var) = do
        casted <- anonInst $ castReference
                    (typeToLLVM $ variableType var)
                    (nameToLLVM $ variableName var)
                    wordTy
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
                            wordTy
                            casted
            , maybeAtomicity = Nothing
            , alignment = 1
            , metadata = [] }
    mkInst _retTy (Address ptr offset) =
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
      header <- anonInst $ Call
          { tailCallKind = Nothing
          , callingConvention = C
          , returnAttributes = []
          , function = Right (ConstantOperand $ Constant.GlobalReference
                                  (FunctionType wordTy [wordTy] False)
                                  (LLVM.Name "_lhc_mkTag"))
          , arguments =
              [ (ConstantOperand (Constant.Int 64 (fromIntegral ident)), []) ]
          , functionAttributes = []
          , metadata = [] }

      return BitCast
          { operand0 = LocalReference wordTy header
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
                            (LLVM.ptr
                                (ArrayType (fromIntegral (length str)+1) i8))
                            strGlobal
            , indices =
                [ ConstantOperand $ Constant.Int 64 0
                , ConstantOperand $ Constant.Int 64 0]
            , metadata = [] }

    mkInst retTy (TypeCast Variable{..}) = return $
        castReference (typeToLLVM variableType) (nameToLLVM variableName) retTy
    mkInst _retTy (Bedrock.Builtin "isIndirection" [PVariable arg]) = do
      pure Call
          { tailCallKind = Nothing
          , callingConvention = C
          , returnAttributes = []
          , function = Right (ConstantOperand $ Constant.GlobalReference
                                  (FunctionType wordTy [LLVM.ptr wordTy] False)
                                  (LLVM.Name "_lhc_isIndirectionP"))
          , arguments =
              [ (LocalReference
                              (typeToLLVM $ variableType arg)
                              (nameToLLVM $ variableName arg), []) ]
          , functionAttributes = []
          , metadata = [] }
    mkInst _retTy (Bedrock.Builtin "getIndirection" [PVariable arg]) = do
      pure Call
          { tailCallKind = Nothing
          , callingConvention = C
          , returnAttributes = []
          , function = Right (ConstantOperand $ Constant.GlobalReference
                                  (FunctionType (LLVM.ptr wordTy) [LLVM.ptr wordTy] False)
                                  (LLVM.Name "_lhc_getIndirectionP"))
          , arguments =
              [ (LocalReference
                              (typeToLLVM $ variableType arg)
                              (nameToLLVM $ variableName arg), []) ]
          , functionAttributes = []
          , metadata = [] }
    mkInst _retTy (Bedrock.Builtin "setIndirection" [PVariable ptr, PVariable newVal]) = do
      pure Call
          { tailCallKind = Nothing
          , callingConvention = C
          , returnAttributes = []
          , function = Right (ConstantOperand $ Constant.GlobalReference
                                  (FunctionType VoidType [LLVM.ptr wordTy, LLVM.ptr wordTy] False)
                                  (LLVM.Name "_lhc_setIndirection"))
          , arguments =
              [ (LocalReference
                              (typeToLLVM $ variableType ptr)
                              (nameToLLVM $ variableName ptr), [])
              , (LocalReference
                              (typeToLLVM $ variableType newVal)
                              (nameToLLVM $ variableName newVal), [])]
          , functionAttributes = []
          , metadata = [] }

    mkInst _retTy expr = error $ "Data.Bedrock.LLVM: Unhandled expression: " ++ show expr
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
    cons PointerType{} PointerType{}     = BitCast
    cons PointerType{} _                 = PtrToInt
    cons _ PointerType{}                 = IntToPtr
    cons (IntegerType a) (IntegerType b) | a > b = Trunc
    cons (IntegerType a) (IntegerType b) | a < b = ZExt
    cons a b
      | bitSize a > bitSize b = Trunc
      | bitSize a < bitSize b = ZExt
    cons _ _                             = BitCast

module Data.Bedrock.LLVMGen where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Char
import           Data.List                          (nub)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Word
import           LLVM.General.AST                   as LLVM
import           LLVM.General.AST.CallingConvention as LLVM
import qualified LLVM.General.AST.Constant          as Constant
import           LLVM.General.AST.Global            as Global
import           LLVM.General.AST.Linkage           as LLVM
import           LLVM.General.AST.Visibility        as LLVM

import           Data.Bedrock                       (NodeName)

data Env = Env
    { envNodeMapping :: Map NodeName Integer
    }

type GenT m a = RWST Env [Definition] Word m a

data GenState = GenState { genUnique :: Word, genScope :: Set String }

type GenModule a = GenT Identity a
type GenBlocks a = GenT (WriterT ([Named Instruction],[BasicBlock])
                          (State GenState)) a

execGenModule :: Env -> GenModule a -> [Definition]
execGenModule env action = nub $ snd $ execRWS action env 0


newDefinition :: Monad m => Definition -> GenT m ()
newDefinition def = tell [def]

genBlocks :: GenBlocks Terminator -> GenModule [BasicBlock]
genBlocks action = RWST $ \env st -> do
  let genState = GenState 1 Set.empty
      ((ret, st', defs), (insts, blocks)) =
        evalState (runWriterT (runRWST action env st)) genState
      startBlock = BasicBlock (UnName 0) insts (Do ret)
  return (startBlock : blocks, st', defs)

newBlock :: GenBlocks Terminator -> GenBlocks Name
newBlock action = do
  name <- anonName
  RWST $ \env st -> WriterT $ do
    ((ret, st', defs), (insts, blocks)) <- runWriterT (runRWST action env st)
    let block = BasicBlock name insts (Do ret)
    return ((name, st', defs), ([], block:blocks))

newTaggedBlock :: String -> GenBlocks Terminator -> GenBlocks Name
newTaggedBlock "" action = newBlock action
newTaggedBlock tag action = do
  name <- taggedName tag
  RWST $ \env st -> WriterT $ do
    ((ret, st', defs), (insts, blocks)) <- runWriterT (runRWST action env st)
    let block = BasicBlock name insts (Do ret)
    return ((name, st', defs), ([], block:blocks))

resolveNodeName :: Monad m => NodeName -> GenT m Integer
resolveNodeName name = asks $ Map.findWithDefault err name . envNodeMapping
  where
    err = error $ "Data.Bedrock.LLVMGen.resolveNodeName: " ++ show name

anonGlobal :: Monad m => Global -> GenT m Name
anonGlobal global = do
  n <- get
  put $ n+1
  let name = Name ("global_"++show n)
  newDefinition $ GlobalDefinition global{ name = name }
  return name

globalString :: Monad m => String -> GenT m Name
globalString str =
  anonGlobal globalVariableDefaults
    { initializer = Just $ Constant.Array (IntegerType 8)
        [ Constant.Int 8 (fromIntegral $ ord c)
        | c <- str++"\0" ]
    , Global.type' = (ArrayType (fromIntegral (length str)+1) (IntegerType 8))
    , linkage = Internal
    , isConstant = True
    , hasUnnamedAddr = True
    }


----------------------------
-- helpers



anonInst :: Instruction -> GenBlocks Name
anonInst inst = do
  name <- anonName
  pushInst (name := inst)
  return name

doInst :: Instruction -> GenBlocks ()
doInst = pushInst . Do

pushInst :: Named Instruction -> GenBlocks ()
pushInst inst = lift $ tell ([inst], [])

anonName :: GenBlocks Name
anonName = lift $ do
  n <- gets genUnique
  modify $ \st -> st{genUnique = n+1}
  return $ UnName n

taggedName :: String -> GenBlocks Name
taggedName tag = lift $ do
  st <- get
  if tag `Set.member` genScope st
    then do
      let n = genUnique st
      put st{genUnique = n+1}
      return $ Name $ tag ++ "_" ++ show n
    else do
      return $ Name tag






----------------------------
-- tests


testModule :: GenModule ()
testModule = do
  let name = Name "blah"
  blocks <- genBlocks testFn
  tell [GlobalDefinition $ functionDefaults
    { name = name
    , returnType = VoidType
    , parameters = ([], False)
    , visibility = Default
    , linkage = Private
    , Global.callingConvention = C
    , basicBlocks = blocks
    }]
  return ()

testFn :: GenBlocks Terminator
testFn = do
  ptr <- anonInst $ GetElementPtr
          { inBounds = True
          , address = LocalReference VoidType (Name "missing")
          , indices = []
          , metadata = [] }
  ptr <- anonInst $ GetElementPtr
          { inBounds = True
          , address = LocalReference VoidType (Name "missing")
          , indices = []
          , metadata = [] }
  return $ Ret Nothing []

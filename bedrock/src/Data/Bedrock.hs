module Data.Bedrock where

import Test.QuickCheck
import Control.Applicative ((<$>), (<*>), pure )
import qualified LLVM.General.AST                   as LLVM

data Name = Name
  { nameModule     :: [String]
  , nameIdentifier :: String
  , nameUnique     :: Int
  } deriving (Show, Eq, Ord)
type NodeSize = Int
data Type
  = NodePtr
  | Node
  | StaticNode NodeSize
  | Primitive CType
  | LLVMPrimitive LLVM.Type
  | FramePtr
  deriving (Show, Eq, Ord)
data Variable = Variable
  { variableName :: Name
  , variableType :: Type
  } deriving (Show, Eq, Ord)

data AvailableNamespace = AvailableNamespace
  { nsNextPointerId   :: Int
  , nsNextNodeId      :: Int
  , nsNextPrimitiveId :: Int
  , nsNextGlobalId    :: Int
  } deriving (Show)

-- XXX: Rename to FFIType? ForeignType?
data CType
  = I8
  | I32
  | I64
  | IWord
  | CPointer CType
  | CVoid
  | CFunction CType [CType]
  -- CFloat
  -- CDouble
  deriving (Show, Eq, Ord)

data Foreign = Foreign
  { foreignName :: String
  , foreignReturn :: CType
  , foreignArguments :: [CType]
  } deriving (Show)

data Module = Module
  { modForeigns :: [Foreign]
  , nodes      :: [NodeDefinition]
  , entryPoint :: Name
  , functions  :: [Function]
  , modNamespace :: AvailableNamespace
  -- CAFs?
  } deriving (Show)

data NodeName
  = ConstructorName Name Int
  | FunctionName Name Int
  | UnboxedTupleName
    -- ^ name of the function and the number of missing arguments.
  deriving (Show, Eq, Ord)

data NodeDefinition = NodeDefinition Name [Type]
  deriving (Show)

data Attribute
  = NoCPS
  | Internal
  deriving (Show,Eq)

data Function = Function
  { fnName       :: Name
  , fnAttributes :: [Attribute]
  , fnArguments  :: [Variable]
  , fnResults    :: [Type]
  , fnBody       :: Block
  } deriving (Show)

data Pattern
  = NodePat NodeName [Variable]
  | LitPat Literal
  -- | UnboxedPat [Variable]
  deriving (Show)
data Alternative = Alternative Pattern Block
  deriving (Show)

data Literal
  = LiteralInt Integer -- compile error if Integer to too large
  | LiteralString String
  deriving (Show, Eq)

data MemAttributes = MemAttributes
  { memConstant :: Bool
  , memAliasGroup :: Maybe Int
  } deriving (Show, Eq)

data Expression
  = Application Name [Variable]
  | CCall String [Variable]
  | Catch Name [Variable] Name [Variable]
  -- Built-in
  | Alloc Int
  | Store NodeName [Variable]
  | BumpHeapPtr Int
  | Write Variable Int Variable
  | Address Variable Int
  | FunctionPointer Name

  | Fetch MemAttributes Variable
  | Load MemAttributes Variable Int
  | Add Variable Variable
  | Undefined
  | Save Variable Int
  | Restore Int

  -- Global variables.
  | ReadRegister String
  | WriteRegister String Variable
  | ReadGlobal String
  | WriteGlobal String Variable
  | TypeCast Variable
  | MkNode NodeName [Variable]
  | Literal Literal

  -- Eval/Apply
  | Eval Variable
  | Apply Variable Variable

  -- GC
  | GCAllocate Int
  | GCBegin
  | GCEnd
  | GCMark Variable
  | GCMarkNode Variable
  deriving (Show)

data Block
  = Case Variable (Maybe Block) [Alternative]
  | Bind [Variable] Expression Block
  | Return [Variable]
  | Raise Variable
  | TailCall Name [Variable]
  | Invoke Variable [Variable]
  | InvokeHandler Variable Variable
  | Exit
  | Panic String
  deriving (Show)





-----------------------------------------------
-- QuickCheck instances

instance Arbitrary Name where
  arbitrary = Name
    <$> listOf moduleName
    <*> variableName
    <*> fmap abs arbitrary
    where
      moduleName = (:) <$> elements ['A'..'Z'] <*> listOf (elements ['a'..'z'])
      variableName = listOf1 (elements ['a'..'z'])

instance Arbitrary Type where
  arbitrary = oneof
    [ elements
      [ NodePtr
      , Node
      , FramePtr ]
    , StaticNode <$> fmap abs arbitrary
    , Primitive <$> arbitrary ]

instance Arbitrary Variable where
  arbitrary = Variable <$> arbitrary <*> arbitrary

instance Arbitrary CType where
  arbitrary = oneof
    [ CPointer <$> arbitrary
    , elements
      [ I8
      , I32
      , I64
      , IWord
      , CVoid ]
    ]

instance Arbitrary Foreign where
  arbitrary = Foreign
    <$> listOf1 (elements ['a'..'z'])
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Module where
  arbitrary = Module
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure (AvailableNamespace 0 0 0 0)

instance Arbitrary NodeName where
  arbitrary = oneof
    [ ConstructorName <$> arbitrary <*> fmap abs arbitrary
    , FunctionName <$> arbitrary <*> fmap abs arbitrary ]

instance Arbitrary NodeDefinition where
  arbitrary = NodeDefinition <$> arbitrary <*> arbitrary

instance Arbitrary Attribute where
  arbitrary = elements [ NoCPS, Internal ]

instance Arbitrary Function where
  arbitrary = Function
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Pattern where
  arbitrary = oneof
    [ NodePat <$> arbitrary <*> arbitrary
    , LitPat <$> arbitrary ]

instance Arbitrary Alternative where
  arbitrary = Alternative <$> arbitrary <*> arbitrary

instance Arbitrary Literal where
  arbitrary = oneof
    [ LiteralInt <$> arbitrary
    , LiteralString <$> arbitrary ]

instance Arbitrary MemAttributes where
  arbitrary = MemAttributes
    <$> arbitrary
    <*> arbitrary

plainIdentifier = listOf1 (elements ['a'..'z'])

instance Arbitrary Expression where
  arbitrary = oneof
    [ Application <$> arbitrary <*> arbitrary
    , CCall <$> plainIdentifier <*> arbitrary
    , Catch <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Alloc <$> arbitrary `suchThat` (>0)
    , Store <$> arbitrary <*> arbitrary
    , Write <$> arbitrary <*> fmap abs arbitrary <*> arbitrary
    , Address <$> arbitrary <*> fmap abs arbitrary
    , Fetch <$> arbitrary <*> arbitrary
    , Load <$> arbitrary <*> arbitrary <*> fmap abs arbitrary
    , Add <$> arbitrary <*> arbitrary
    , ReadRegister <$> plainIdentifier
    , WriteRegister <$> plainIdentifier <*> arbitrary
    , ReadGlobal <$> plainIdentifier
    , WriteGlobal <$> plainIdentifier <*> arbitrary
    , TypeCast <$> arbitrary
    , MkNode <$> arbitrary <*> arbitrary
    , Literal <$> arbitrary
    , Eval <$> arbitrary
    , Apply <$> arbitrary <*> arbitrary
    -- , GCAllocate
    -- , GCBegin
    -- , GCEnd
    -- , GCMark
    -- , GCMarkNode
    ]

instance Arbitrary Block where
  arbitrary = oneof
    [ Case <$> arbitrary <*> arbitrary <*> arbitrary
    , Bind <$> arbitrary <*> arbitrary <*> arbitrary
    , Return <$> arbitrary
    , Raise <$> arbitrary
    , TailCall <$> arbitrary <*> arbitrary
    , Invoke <$> arbitrary <*> arbitrary
    , InvokeHandler <$> arbitrary <*> arbitrary
    , pure Exit
    , Panic <$> arbitrary ]


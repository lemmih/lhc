{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-|
Bedrock is a strict, first-order language designed for graph reduction.

The language supports high-level features such as graph reduction (via 'eval'
and 'apply' primitives), an implicit stack (via function application and function
return), exceptions (via the 'catch', 'raise' and 'invoke' primitives), automatic
memory management (via the 'store' and 'alloc' primitives). Each of these
primitives can be removed by code transformations that maintain the semantic
properties of the code.

The code transformations work roughly like this:

 * Lowering 'eval' and 'apply' using heap-points-to (HPT) analysis.
   HPT analysis yields a conservative set of possible nodes for each variable in
   the bedrock code. Appropriate 'eval' functions and 'apply' functions can then
   be generated for each use of the 'eval' and 'apply' primitives. An 'eval'
   function might look like this:

   > eval ptr = do
   >   node <- fetch ptr
   >   ret <- case node of
   >     FunctionNode arg1 arg2 -> function arg1 arg2
   >   update ptr (Indirection ret)
   >   return ret

 * Lowering 'eval' and 'apply' can also be done without an HPT analysis (this is
   approach currently implemented). This is necessarily much more conservative.
   The bedrock code is scanned once to find all possible suspected functions and
   all possible function applications by type. A single @eval@ function is then
   generated and 'apply' functions are generated for each argument type / return
   type pair. For example:

@
   apply_i8 ptr i8 = do
     node <- fetch ptr
     case node of
       Function arg1 _ -> function arg1 i8
       PartialFunction _ _ -> return (PartialFunction i8 _)
@

  * The stack primitives are removed by a CPS transformation. Activation frames
    are made explicit and can be traversed by the exception handling code.
    For example:

@
    function = do
      a <- callFnA
      b <- callFnB
      store (Tuple a b)
@

    Would be transformed into:

@
    function = do
      push function_part2
      tailcall callFnA
    function_part2 a =
      push (function_part3 a)
      tailcall callFnB
    function_part3 a b =
      store (Tuple a b)
@

  * Memory management primitives are removed by plugging in memory management
    library. Right now the only library available just allocations a large slab
    of memory when executables boot up and never performs any garbage collection.

Once all the high-level primitives have been transformed out of the bedrock code,
LLVM code can be trivially generated.
-}
module Data.Bedrock where

import           Control.Applicative (pure, (<$>), (<*>))
import           Data.Data
import           GHC.Generics
import qualified LLVM.AST.Type       as LLVM
import           Test.QuickCheck     (Arbitrary (..), elements, listOf, listOf1,
                                      oneof, suchThat)

data Name = Name
  { nameModule     :: [String]
  , nameIdentifier :: String
  , nameUnique     :: Int
  } deriving (Show, Read, Eq, Ord, Data, Generic)
type NodeSize = Int
data Type
  = NodePtr
  | Node
  | StaticNode NodeSize
  | IWord
  | Primitive CType
  | LLVMPrimitive LLVM.Type
  | FramePtr
  deriving (Show, Read, Eq, Ord, Data, Generic)
data Variable = Variable
  { variableName :: Name
  , variableType :: Type
  } deriving (Show, Read, Eq, Ord, Data, Generic)

data AvailableNamespace = AvailableNamespace
  { nsNextPointerId   :: Int
  , nsNextNodeId      :: Int
  , nsNextPrimitiveId :: Int
  , nsNextGlobalId    :: Int
  } deriving (Show, Read, Eq, Data, Generic)

-- XXX: Rename to FFIType? ForeignType?
data CType
  = I8
  | I32
  | I64
  | CPointer CType
  | CVoid
  | CFunction CType [CType]
  -- CFloat
  -- CDouble
  deriving (Show, Read, Eq, Ord, Data, Generic)

data Foreign = Foreign
  { foreignName      :: String
  , foreignReturn    :: CType
  , foreignArguments :: [CType]
  } deriving (Show, Read, Eq, Data, Generic)

data Module = Module
  { modForeigns  :: [Foreign]
  , nodes        :: [NodeDefinition]
  , entryPoint   :: Name
  , functions    :: [Function]
  , modNamespace :: AvailableNamespace
  -- CAFs?
  } deriving (Show, Read, Eq, Data, Generic)

data NodeName
  = ConstructorName Name Int
  | FunctionName Name Int
  | UnboxedTupleName
    -- ^ name of the function and the number of missing arguments.
  deriving (Show, Read, Eq, Ord, Data, Generic)

data NodeDefinition = NodeDefinition Name [Type]
  deriving (Show, Read, Eq, Data, Generic)

data Attribute
  = NoCPS
  | Internal
  deriving (Show, Read, Eq, Data, Generic)

data Function = Function
  { fnName       :: Name
  , fnAttributes :: [Attribute]
  , fnArguments  :: [Variable]
  , fnResults    :: [Type]
  , fnBody       :: Block
  } deriving (Show, Read, Eq, Data, Generic)

data Pattern
  = NodePat NodeName [Variable]
  | LitPat Literal
  -- UnboxedPat [Variable]
  deriving (Show, Read, Eq, Data, Generic)
data Alternative = Alternative Pattern Block
  deriving (Show, Read, Eq, Data, Generic)

data Literal
  = LiteralInt Integer -- compile error if Integer to too large
  | LiteralString String
  deriving (Show, Read, Eq, Data, Generic)

data MemAttributes = MemAttributes
  { memConstant   :: Bool
  , memAliasGroup :: Maybe Int
  } deriving (Show, Read, Eq, Data, Generic)

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
  deriving (Show, Read, Eq, Data, Generic)

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
  deriving (Show, Read, Eq, Data, Generic)





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
      , FramePtr
      , IWord ]
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

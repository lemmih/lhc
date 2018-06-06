{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import qualified LLVM.AST            as LLVM (Type (..), mkName)
import qualified LLVM.AST.Type       as LLVM
import           Test.QuickCheck     hiding (Function)

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
  | Primitive LLVM.Type
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

-- -- XXX: Rename to FFIType? ForeignType?
-- data CType
--   = I8
--   | I32
--   | I64
--   | CPointer CType
--   | CVoid
--   | CFunction CType [CType]
--   -- CFloat
--   -- CDouble
--   deriving (Show, Read, Eq, Ord, Data, Generic)

data Foreign = Foreign
  { foreignName      :: String
  , foreignReturn    :: LLVM.Type
  , foreignArguments :: [LLVM.Type]
  } deriving (Show, Read, Eq, Data, Generic)

data Module = Module
  { modForeigns  :: [Foreign]
  , nodes        :: [NodeDefinition]
  , modLayouts   :: [NodeLayout]
  , entryPoint   :: Name
  , functions    :: [Function]
  , modNamespace :: AvailableNamespace
  -- CAFs?
  } deriving (Show, Read, Eq, Data, Generic)

data NodeLayout = NodeLayout
  { layoutName       :: NodeName
  , layoutPrimitives :: Int
  , layoutPointers   :: Int
  } deriving (Show, Read, Eq, Ord, Data, Generic)

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
  | Prefix Int Int Int (Maybe Name) -- size, primitives, pointers, exception handler
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

data Parameter
  = PInt Int
  | PString String
  | PName Name
  | PNodeName NodeName
  | PVariable Variable
  | PVariables [Variable]
  deriving (Read, Show, Eq, Data, Generic)

data Expression
  = Application Name [Variable]
  | CCall String [Variable]
  | Catch Name [Variable] Name [Variable]
  | InvokeReturn Variable [Variable]
  | Literal Literal
  -- Built-in
  | Builtin String [Parameter]


  deriving (Show, Read, Eq, Data, Generic)

pattern TypeCast var = Builtin "cast" [PVariable var]
pattern MkNode name args = Builtin "node" [PNodeName name, PVariables args]
pattern Eval a = Builtin "eval" [PVariable a]
pattern Apply a b = Builtin "apply" [PVariable a, PVariable b]
pattern Store node args = Builtin "store" [PNodeName node, PVariables args]
pattern Alloc n = Builtin "alloc" [PInt n]
pattern BumpHeapPtr n = Builtin "bump" [PInt n]

pattern Write ptr idx var = Builtin "write" [PVariable ptr, PInt idx, PVariable var]
pattern Address ptr idx = Builtin "address" [PVariable ptr, PInt idx]
pattern FunctionPointer name = Builtin "fn_ptr" [PName name]

pattern Fetch ptr = Builtin "fetch" [PVariable ptr]
pattern Load ptr idx = Builtin "load" [PVariable ptr, PInt idx]
pattern Undefined = Builtin "undefined" []
pattern Save var slot = Builtin "save" [PVariable var, PInt slot]
pattern Restore slot = Builtin "restore" [PInt slot]

pattern ReadRegister reg = Builtin "read_register" [PString reg]
pattern WriteRegister reg var = Builtin "write_register" [PString reg, PVariable var]
pattern ReadGlobal reg = Builtin "read_global" [PString reg]
pattern WriteGlobal reg var = Builtin "write_global" [PString reg, PVariable var]

pattern GCAllocate n = Builtin "gc_allocate" [PInt n]
pattern GCBegin = Builtin "gc_begin" []
pattern GCEnd = Builtin "gc_end" []
pattern GCMark var = Builtin "gc_mark" [PVariable var]
pattern GCMarkNode var = Builtin "gc_mark_node" [PVariable var]
pattern GCMarkFrame var = Builtin "gc_mark_frame" [PVariable var]

data Block
  = Case Variable (Maybe Block) [Alternative]
  | Bind [Variable] Expression Block
  | Return [Variable]
  | Raise Variable
  | TailCall Name [Variable]
  | Invoke Variable [Variable]
  | Exit
  | Panic String
  deriving (Show, Read, Eq, Data, Generic)


wordTy :: LLVM.Type
wordTy = LLVM.NamedTypeReference (LLVM.mkName "word")



-----------------------------------------------
-- QuickCheck instances

upperString :: Gen String
upperString = (:) <$> elements ['A'..'Z'] <*> listOf (elements ['a'..'z'])

upperName :: Gen Name
upperName = Name <$> listOf upperString <*> upperString <*> fmap abs arbitrary

instance Arbitrary Name where
  arbitrary = Name
    <$> listOf upperString
    <*> listOf1 (elements ['a'..'z'])
    <*> fmap abs arbitrary

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

recursive :: Gen a -> Gen a -> Gen a
recursive deep shallow =
  sized $ \case
    0 -> shallow
    n -> resize (n`div`2) $ oneof [deep, shallow]

instance Arbitrary LLVM.Type where
  arbitrary =
    recursive
      (LLVM.FunctionType <$> base <*> arbitrary <*> pure False)
      base
    where
      base =
        recursive
          (LLVM.ptr <$> base)
          (elements
            [ LLVM.i8
            , LLVM.i32
            , LLVM.i64
            , LLVM.VoidType ])

instance Arbitrary Foreign where
  arbitrary = Foreign
    <$> listOf1 (elements ['a'..'z'])
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Module where
  arbitrary = sized (\n -> resize (n`div`10) (Module
    <$> arbitrary
    <*> arbitrary
    <*> pure []
    <*> arbitrary
    <*> arbitrary
    <*> pure (AvailableNamespace 0 0 0 0)))

instance Arbitrary NodeName where
  arbitrary = oneof
    [ ConstructorName <$> upperName <*> fmap abs arbitrary
    , FunctionName <$> arbitrary <*> fmap abs arbitrary ]

instance Arbitrary NodeDefinition where
  arbitrary = NodeDefinition <$> upperName <*> arbitrary

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

instance Arbitrary Parameter where
  arbitrary = oneof
    [ PInt <$> arbitrary
    , PString <$> arbitrary
    , PName <$> arbitrary
    , PNodeName <$> arbitrary
    , PVariable <$> arbitrary
    , PVariables <$> arbitrary ]

plainIdentifier :: Gen String
plainIdentifier = listOf1 (elements ['a'..'z'])

instance Arbitrary Expression where
  arbitrary = oneof
    [ Application <$> arbitrary <*> arbitrary
    , CCall <$> plainIdentifier <*> arbitrary
    , Catch <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Alloc <$> arbitrary `suchThat` (>0)
    , Store <$> arbitrary <*> arbitrary
    -- , Write <$> arbitrary <*> fmap abs arbitrary <*> arbitrary
    , Address <$> arbitrary <*> fmap abs arbitrary
    , Fetch <$> arbitrary
    , Load <$> arbitrary <*> fmap abs arbitrary
    -- , Add <$> arbitrary <*> arbitrary
    -- , ReadRegister <$> plainIdentifier
    -- , WriteRegister <$> plainIdentifier <*> arbitrary
    -- , ReadGlobal <$> plainIdentifier
    -- , WriteGlobal <$> plainIdentifier <*> arbitrary
    , TypeCast <$> arbitrary
    , MkNode <$> arbitrary <*> arbitrary
    , Literal <$> arbitrary
    -- , GCAllocate
    -- , GCBegin
    -- , GCEnd
    -- , GCMark
    -- , GCMarkNode
    ]

instance Arbitrary Block where
  shrink (Case _scrut (Just def) alts) =
    def : [ block | Alternative _ block <- alts ]
  shrink (Case _scrut Nothing alts) =
    [ block | Alternative _ block <- alts ]
  shrink (Bind _ _ rest) = [rest]
  shrink _ = []
  arbitrary = recursive (oneof
    [ Case <$> arbitrary <*> arbitrary <*> arbitrary
    , Bind <$> arbitrary <*> arbitrary <*> arbitrary
    , Return <$> arbitrary
    , Raise <$> arbitrary
    , TailCall <$> arbitrary <*> arbitrary
    , Invoke <$> arbitrary <*> arbitrary
    , pure Exit
    , Panic <$> arbitrary ]) (pure Exit)

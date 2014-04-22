module Data.Bedrock where

data Name = Name
	{ nameModule     :: [String]
	, nameIdentifier :: String
	, nameUnique     :: Int
	} deriving (Show, Eq, Ord)
type NodeSize = Int
data Type = NodePtr | Node | StaticNode NodeSize | Primitive
	deriving (Show, Eq, Ord)
data Variable = Variable
	{ variableName :: Name
	, variableType :: Type
	} deriving (Show, Eq, Ord)

data AvailableNamespace = AvailableNamespace
	{ nsNextPointerId   :: Int
	, nsNextNodeId      :: Int
	, nsNextPrimitiveId :: Int
	, nsNextGlobalId    :: Int }

data CType
	= I8
	| I32
	| I64
	| CPointer CType
	| CVoid
	deriving (Show)

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
	}

data NodeName
	= ConstructorName Name
	| FunctionName Name Int
		-- ^ name of the function and the number of missing arguments.
	deriving (Show, Eq, Ord)

data NodeDefinition = NodeDefinition Name [Type]
	deriving (Show)

data Function = Function
	{ fnName      :: Name
	, fnArguments :: [Variable]
	, fnResults   :: [Type]
	, fnBody      :: Block
	} deriving (Show)

data Pattern
	= NodePat NodeName [Variable]
	| LitPat Literal
	deriving (Show)
data Alternative = Alternative Pattern Block
	deriving (Show)

data Literal
	= LiteralInt Integer -- compile error if Integer to too large
	| LiteralString String
	deriving (Show, Eq)

data Argument
	= RefArg Variable
	| LitArg Literal
	| NodeArg NodeName [Variable]
	deriving (Show)

data Expression
	= Literal Literal
	| Application Name [Variable]
	| CCall String [Variable]
	| WithExceptionHandler Name [Variable] Name [Variable]
	-- Built-in
	| Alloc Int
	| SizeOf NodeName [Variable]
	| Store NodeName [Variable]
	| Write Variable Int Argument
	| Address Variable Int

	| Fetch Variable
	| Load Variable Int
	| Add Variable Variable

	-- Global variables.
	| ReadRegister String
	| WriteRegister String Variable
	| ReadGlobal String
	| WriteGlobal String Variable
	| Unit Argument

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
	| Throw Variable
	| TailCall Name [Variable]
	| Invoke Variable [Variable]
	| Exit
	| Panic String
	deriving (Show)


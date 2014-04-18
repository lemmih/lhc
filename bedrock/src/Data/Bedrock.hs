module Data.Bedrock where

data Name = Name
	{ nameModule     :: [String]
	, nameIdentifier :: String
	, nameUnique     :: Int
	} deriving (Show, Eq, Ord)
data Type = NodePtr | Node | Primitive
	deriving (Show, Eq, Ord)
data Variable = Variable
	{ variableName :: Name
	, variableType :: Type
	} deriving (Show, Eq, Ord)

data Module = Module
	{ nodes     :: [NodeDefinition]
	, functions :: [Function]
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
	, fnBody      :: Expression
	} deriving (Show)

data Pattern
	= NodePat NodeName [Variable]
	| LitPat Literal
	deriving (Show)
data Alternative = Alternative Pattern Expression
	deriving (Show)

data Literal
	= LiteralInt Integer -- compile error if Integer to too large
	deriving (Show, Eq)

data Argument
	= RefArg Variable
	| LitArg Literal
	| NodeArg NodeName [Variable]
	deriving (Show)

data SimpleExpression
	= Literal Literal
	| Application Name [Variable]
	| WithExceptionHandler Name [Variable] Name [Variable]
	-- Built-in
	| Alloc Int
	| SizeOf NodeName [Variable]
	| Store NodeName [Variable]
	| Fetch Variable
	| Load Variable Int
	| Add Variable Variable
	| Print Variable
	| ReadGlobal String
	| WriteGlobal String Variable
	| Unit [Argument]
	-- GC
	| GCAllocate Int
	| GCBegin
	| GCEnd
	| GCMark Variable
	| GCMarkNode Variable
	deriving (Show)

data Expression
	= Case Variable (Maybe Expression) [Alternative]
	| Bind [Variable] SimpleExpression Expression
	| Return [Variable]
	| Throw Variable
	| TailCall Name [Variable]
	| Invoke Variable [Variable]
	| Exit
	| Panic String
	deriving (Show)


module Data.Bedrock where

data Name = Name
	{ nameModule     :: [String]
	, nameIdentifier :: String
	, nameUnique     :: Int
	} deriving (Show, Eq, Ord)
data Type = NodePtr | RawNode | Primitive | MissingType
	deriving (Show, Eq, Ord)
data Variable = Variable
	{ variableName :: Name
	, variableType :: Type
	} deriving (Show, Eq, Ord)

data Module = Module
	{ nodes     ::     [Node]
	, functions :: [Function]
	-- CAFs?
	}

data NodeName
	= ConstructorName Name
	| FunctionName Name Int
		-- ^ name of the function and the number of missing arguments.
	deriving (Show, Eq)

data Node = Node
	deriving (Show)

data Function = Function
	{ fnName      :: Name
	, fnArguments :: [Variable]
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
	| NodeArg NodeName [Argument]
	deriving (Show)

data SimpleExpression
	= Literal Literal
	| Application Name [Argument]
	| WithExceptionHandler Name [Argument] Name [Argument]
	-- Built-in
	| Alloc Int
	| Store NodeName [Argument]
	| Fetch Variable
	| Load Variable Int
	| Add Argument Argument
	| Print Variable
	deriving (Show)

data Expression
	= Case Variable [Alternative] (Maybe Expression)
	| Bind [Variable] SimpleExpression Expression
	| Return [Argument]
	| Throw Argument
	| TailCall Name [Argument]
	| Invoke Variable [Argument]
	| Exit
	deriving (Show)


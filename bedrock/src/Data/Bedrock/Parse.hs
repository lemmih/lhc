module Data.Bedrock.Parse where

import           Control.Applicative                    (pure, (<$>), (<*>))
import           Control.Monad                          (guard)
import           Data.Char
import           Data.Maybe
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Language as P
import qualified Text.ParserCombinators.Parsec.Token    as P

import           Data.Bedrock

-------------------------------------------------------------------------------
-- Parsing

bedrockDef :: P.LanguageDef ()
bedrockDef = P.emptyDef
	{ P.commentLine = ";" -- similar to LLVM IR
	, P.caseSensitive = True
	, P.reservedNames =
			[ "node", "foreign", "entrypoint", "case", "of" ]
	, P.reservedOpNames = ["=","->"]
	-- , P.identStart = P.identStart P.emptyDef <|> char '@'
	}

lexer :: P.TokenParser ()
lexer = P.makeTokenParser bedrockDef

natural, integer :: Parser Integer
identifier :: Parser String
symbol :: String -> Parser String
parens :: Parser a -> Parser a
reservedOp, reserved :: String -> Parser ()
stringLiteral, comma :: Parser String
commaSep, commaSep1 :: Parser a -> Parser [a]

identifier = P.identifier lexer
parens     = P.parens lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
symbol     = P.symbol lexer
comma      = P.comma lexer
integer    = P.integer lexer
stringLiteral = P.stringLiteral lexer
commaSep   = P.commaSep lexer
commaSep1  = P.commaSep1 lexer
natural    = P.natural lexer

parseName :: Parser Name
parseName =
	Name
		<$> pure []
		<*> identifier
		<*> pure 0

parseType :: Parser Type
parseType = choice
	[ symbol "*" >> return NodePtr
	, symbol "%" >> return Node
	, symbol "#" >> return (Primitive IWord)
	, Primitive <$> parseCType ]
	<?> "type"

parseVariable :: Parser Variable
parseVariable = do
	ty <- parseType
	name <- parseName
	return Variable{ variableName = name, variableType = ty }

parseNodeDefinition :: Parser NodeDefinition
parseNodeDefinition = do
	reserved "node"
	name <- parseName
	args <- many parseType
	return $ NodeDefinition name args

parseFunction :: Parser Function
parseFunction = do
	retTypes <- many parseType
	name <- parseName
	args <- parens (commaSep parseVariable)
	body <- parseBlock
	return (Function name args retTypes body)

-- FIXME: Parse nodes
parseArgument :: Parser Argument
parseArgument =
	RefArg <$> parseVariable <|>
	LitArg <$> parseLiteral <|>
	(do
		constructor <- parseConstructor
		binds <- many parseVariable
		return $ NodeArg (ConstructorName constructor) binds) <|>
	(do
		fn <- parseName
		binds <- many parseVariable
		blanks <- many (symbol "_")
		return $ NodeArg (FunctionName fn (length blanks)) binds)
	<?> "argument"

parseConstructor :: Parser Name
parseConstructor = try (do
	name <- parseName
	guard (isUpper (head (nameIdentifier name)))
	return name)
	<?> "constructor"

parseLiteral :: Parser Literal
parseLiteral =
	LiteralInt <$> integer <|>
	LiteralString <$> stringLiteral
	<?> "literal"

parsePattern :: Parser Pattern
parsePattern = choice
	[ do
		constructor <- parseConstructor
		binds <- many parseVariable
		return $ NodePat (ConstructorName constructor) binds
	, do
		fn <- parseName
		binds <- many parseVariable
		blanks <- many (symbol "_")
		return $ NodePat (FunctionName fn (length blanks)) binds
	, LitPat <$> parseLiteral
	]

parseAlternative :: Parser Alternative
parseAlternative = do
	pat <- parsePattern
	reservedOp "->"
	expression <- parseBlock
	return $ Alternative pat expression

parseExpression :: Parser Expression
parseExpression = choice
	[ do
		reserved "@unit"
		arg <- parens parseArgument
		return $ Unit arg
	, do
		reserved "@alloc"
		n <- natural
		return $ Alloc (fromIntegral n)
	, do
		reserved "@store"
		parens $ do
			constructor <- parseConstructor
			args <- many parseVariable
			return $ Store (ConstructorName constructor) args
		  <|> do
		  	fn <- parseName
		  	args <- many parseVariable
		  	blanks <- many (symbol "_")
		  	return $ Store (FunctionName fn (length blanks)) args
	, do
		reserved "@fetch"
		ptr <- parseVariable
		return $ Fetch ptr
	, do
		reserved "@eval"
		ptr <- parens parseVariable
		return $ Eval ptr
	, do
		reserved "@apply"
		parens $ do
			ptr <- parseVariable
			comma
			val <- parseVariable
			return $ Apply ptr val
	, do
		reserved "@ccall"
		fn <- parseForeignName
		args <- parens (commaSep parseVariable)
		return $ CCall fn args
	, do
		reserved "@catch"
		exh <- parseName
		exhArgs <- parens (commaSep parseVariable)
		fn <- parseName
		args <- parens (commaSep parseVariable)
		return $ Catch exh exhArgs fn args
	, do
		reserved "@add"
		lhs <- parseVariable
		rhs <- parseVariable
		return $ Add lhs rhs
	, do
		fn <- parseName
		args <- parens (commaSep parseVariable)
		return $ Application fn args
	]

parseBlock :: Parser Block
parseBlock = choice (
	[ do
		reserved "@return"
		args <- parens (commaSep parseVariable)
		return $ Return args
	, do
		reserved "case"
		scrut <- parseVariable
		reserved "of"
		alts <- many parseAlternative
		return $ Case scrut Nothing alts
	, do
		names <- commaSep1 parseVariable
		symbol "="
		simple <- parseExpression
		rest <- parseBlock
		return $ Bind names simple rest
	, do
		simple <- parseExpression
		rest <- parseBlock
		return $ Bind [] simple rest
	, do
		reserved "@raise"
		e <- parens parseVariable
		return $ Raise e
	, do
		reserved "@exit"
		return $ Exit
	, do
		reserved "@tail"
		fn <- parseName
		args <- parens (commaSep parseVariable)
		return $ TailCall fn args
	])

parseEntryPoint :: Parser Name
parseEntryPoint = do
	reserved "entrypoint"
	symbol ":"
	parseName

-- i8 i32 i64 void i64*
parseCType :: Parser CType
parseCType = do
	ty <- prim
	p <- length <$> many (symbol "*")
	return $ foldr (.) id (replicate p CPointer) ty
  where
  	prim = choice
		[ reserved "i8" >> return I8
		, reserved "i32" >> return I32
		, reserved "i64" >> return I64
		, reserved "void" >> return CVoid
		]

parseForeignName :: Parser String
parseForeignName = identifier

-- foreign [ret] name([args])
parseForeign :: Parser Foreign
parseForeign = do
	reserved "foreign"
	ret <- parseCType
	name <- parseForeignName
	args <- parens $ commaSep parseCType
	return $ Foreign name ret args

data TopLevel
	= TopForeign Foreign
	| TopNode NodeDefinition
	| TopEntryPoint Name
	| TopFunction Function

parseTopLevel :: Parser TopLevel
parseTopLevel =
	TopForeign <$> parseForeign <|>
	TopNode <$> parseNodeDefinition <|>
	TopEntryPoint <$> parseEntryPoint <|>
	TopFunction <$> parseFunction

parseModule :: Parser Module
parseModule = do
	topLevel <- many parseTopLevel
	return Module
		{ modForeigns = [ f | TopForeign f <- topLevel ]
		, nodes = [ n | TopNode n <- topLevel ]
		, entryPoint = fromMaybe noEntryPoint $ listToMaybe
			[ e | TopEntryPoint e <- topLevel ]
		, functions = [ f | TopFunction f <- topLevel ]
		, modNamespace = error "next free unique not defined"
		}
  where
  	noEntryPoint = error "No entrypoint defined."

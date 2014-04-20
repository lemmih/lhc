module Data.Bedrock.Parse where

import           Control.Applicative           (pure, (*>), (<$>), (<*), (<*>))
import           Control.Monad                 (guard)
import           Data.Char
import           Text.ParserCombinators.Parsec

import Data.Bedrock

-------------------------------------------------------------------------------
-- Parsing

parseName :: (Char -> Bool) -> Parser Name
parseName firstLetter =
	Name
		<$> pure []
		<*> ((:) <$> satisfy firstLetter <*> (many alphaNum))
		<*> pure 0

parseType :: Parser Type
parseType = choice (map try
	[ char '*' >> return NodePtr
	, char '%' >> return Node
	, char '#' >> return Primitive ])
	<?> "type"

parseVariable :: Parser Variable
parseVariable = do
	ty <- parseType
	name <- parseName isLower
	return Variable{ variableName = name, variableType = ty }

parseNodeDefinition :: Parser NodeDefinition
parseNodeDefinition = try $ do
	string "node"; spaces
	name <- parseName isUpper; spaces
	args <- many (parseType <* spaces)
	return $ NodeDefinition name args

parseFunction :: Parser Function
parseFunction = do
	retTypes <- many parseType <* spaces
	name <- parseName isLower <* spaces
	args <- parseVariable `endBy` spaces
	char '='; spaces
	body <- parseExpression
	return (Function name args retTypes body)

-- FIXME: Parse nodes
parseArgument :: Parser Argument
parseArgument =
	RefArg <$> try parseVariable <|>
	LitArg <$> parseLiteral <|>
	(parens $ do
		constructor <- parseConstructor <* spaces
		binds <- many (spaces *> parseVariable <* spaces)
		return $ NodeArg (ConstructorName constructor) binds) <|>
	(parens $ do
		fn <- parseName isLower <* spaces
		binds <- many (spaces *> parseVariable <* spaces)
		blanks <- many (spaces *> char '_' <* spaces)
		return $ NodeArg (FunctionName fn (length blanks)) binds)
	<?> "argument"

parseArguments :: Parser [Argument]
parseArguments = (spaces *> parseArgument <* spaces) `sepBy` char ','

--parseNames :: Parser [Name]
--parseNames = (spaces *> parseName isLower <* spaces) `sepBy` char ','

parseVariables :: Parser [Variable]
parseVariables = (spaces *> parseVariable <* spaces) `sepBy` char ','

parseConstructor :: Parser Name
parseConstructor = try (do
	name <- parseName isUpper
	return name)
	<?> "constructor"

parseLiteral :: Parser Literal
parseLiteral =
	LiteralInt . read <$> many1 digit <|>
	LiteralString <$> between (char '"') (char '"') (many (satisfy (/='"')))
	<?> "literal"

parsePattern :: Parser Pattern
parsePattern = choice (map try
	[ do
		constructor <- parseConstructor <* spaces
		binds <- many (spaces *> parseVariable <* spaces)
		return $ NodePat (ConstructorName constructor) binds
	, do
		fn <- parseName isLower <* spaces
		binds <- many (spaces *> parseVariable <* spaces)
		blanks <- many (spaces *> char '_' <* spaces)
		return $ NodePat (FunctionName fn (length blanks)) binds
	, LitPat <$> parseLiteral
	])

parseAlternative :: Parser Alternative
parseAlternative = do
	pat <- parsePattern <* spaces
	string "->"; spaces
	expression <- parseExpression
	return $ Alternative pat expression

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

parseSimpleExpression :: Parser SimpleExpression
parseSimpleExpression = choice
	[ do
		try (string "@unit"); spaces
		args <- parens parseArguments; spaces
		return $ Unit args
	, do
		try (string "@alloc"); spaces
		n <- many1 digit
		return $ Alloc (read n)
	, do
		try (string "@store"); spaces
		parens $ do
			constructor <- parseConstructor <* spaces
			args <- parseVariables
			return $ Store (ConstructorName constructor) args
		  <|> do
		  	fn <- parseName isLower <* spaces
		  	args <- parseVariables
		  	blanks <- many (spaces *> char '_' <* spaces)
		  	return $ Store (FunctionName fn (length blanks)) args
	, do
		try (string "@sizeOf"); spaces
		parens $ do
			constructor <- parseConstructor <* spaces
			args <- parseVariables
			return $ SizeOf (ConstructorName constructor) args
		  <|> do
		  	fn <- parseName isLower <* spaces
		  	args <- parseVariables
		  	blanks <- many (spaces *> char '_' <* spaces)
		  	return $ SizeOf (FunctionName fn (length blanks)) args
	, do
		try (string "@fetch"); spaces
		ptr <- parseVariable <* spaces
		return $ Fetch ptr
	, do
		try (string "@eval"); spaces
		ptr <- parseVariable <* spaces
		return $ Eval ptr
	, do
		try (string "@apply"); spaces
		ptr <- parseVariable <* spaces
		val <- parseVariable <* spaces
		return $ Apply ptr val
	, do
		try (string "@print"); spaces
		var <- parseVariable <* spaces
		return $ Print var
	, do
		fn <- parseName isLower <* spaces
		args <- parens parseVariables
		return $ Application fn args
	, do
		try (string "@withExceptionHandler"); spaces
		exh <- parseName isLower <* spaces
		exhArgs <- parens parseVariables <* spaces
		fn <- parseName isLower <* spaces
		args <- parens parseVariables
		return $ WithExceptionHandler exh exhArgs fn args
	, do
		try (string "@add"); spaces
		lhs <- parseVariable <* spaces
		rhs <- parseVariable <* spaces
		return $ Add lhs rhs
	]

parseExpression :: Parser Expression
parseExpression = spaces *> choice (map try
	[ do
		try (string "@return"); spaces
		args <- parens parseVariables; spaces
		return $ Return args
	, do
		try (string "case"); spaces
		scrut <- parseVariable <* spaces
		string "of"; spaces
		alts <- (spaces *> parseAlternative <* spaces) `sepBy` char '|'
		return $ Case scrut Nothing alts
	, do
		names <- parseVariables
		guard (not (null names))
		string ":="; spaces
		simple <- parseSimpleExpression
		char ';'
		rest <- parseExpression
		return $ Bind names simple rest
	, do
		simple <- parseSimpleExpression
		char ';'
		rest <- parseExpression
		return $ Bind [] simple rest
	, do
		try (string "@throw"); spaces
		e <- parseVariable
		return $ Throw e
	, do
		try (string "@exit"); spaces
		return $ Exit
	, do
		try (string "@tail"); space
		fn <- parseName isLower <* spaces
		args <- parens parseVariables
		return $ TailCall fn args
	]) <?> "expression"

parseEntryPoint :: Parser Name
parseEntryPoint = do
	string "entrypoint"; spaces
	char ':'; spaces
	parseName isLower

-- i8 i32 i64 void i64*
parseCType :: Parser CType
parseCType = do
	ty <- prim
	p <- length <$> many (char '*')
	return $ foldr (.) id (replicate p CPointer) ty
  where
  	prim = choice $ map try
		[ string "i8" >> return I8
		, string "i32" >> return I32
		, string "i64" >> return I64
		, string "void" >> return CVoid
		]

-- foreign [ret] name([args])
parseForeign :: Parser Foreign
parseForeign = do
	string "foreign"; spaces
	ret <- parseCType <* spaces
	name <- many1 alphaNum <* spaces
	args <- parens $
				spaces *>
					(parseCType `sepBy` (try $ spaces >> char ',' >> spaces))
				 <* spaces
	spaces
	return $ Foreign name ret args

parseModule :: Parser Module
parseModule =
	Module
		<$> many parseForeign
		<*> many parseNodeDefinition
		<*> parseEntryPoint
		<*> many parseFunction
		<*> pure (error "next free unique not defined")
		<* eof

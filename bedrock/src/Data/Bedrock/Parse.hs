module Data.Bedrock.Parse where

import           Control.Applicative           (pure, (*>), (<$>), (<*), (<*>))
import           Control.Monad                 (guard)
import           Data.Char
import           Text.ParserCombinators.Parsec

import Data.Bedrock

-------------------------------------------------------------------------------
-- Parsing

parseName :: Parser Name
parseName = Name <$> pure [] <*> many1 alphaNum <*> pure 0

parseType :: Parser Type
parseType = choice (map try
	[ string "ptr" >> return NodePtr
	, string "node" >> return RawNode
	, string "prim" >> return Primitive ])
	<?> "type"

parseVariable :: Parser Variable
parseVariable = do
	name <- parseName
	guard (isLower (head (nameIdentifier name)))
	ty <- (char ':' *> parseType) <|> return NodePtr
	return Variable{ variableName = name, variableType = ty }

parseFunction :: Parser Function
parseFunction = do
	name <- parseName <* spaces
	args <- parseVariable `endBy` spaces
	char '='; spaces
	body <- parseExpression
	return (Function name args body)

-- FIXME: Parse nodes
parseArgument :: Parser Argument
parseArgument =
	RefArg <$> try parseVariable <|>
	LitArg <$> parseLiteral <|>
	(parens $ do
		constructor <- parseConstructor <* spaces
		binds <- many (spaces *> parseArgument <* spaces)
		return $ NodeArg (ConstructorName constructor) binds) <|>
	(parens $ do
		fn <- parseName <* spaces
		binds <- many (spaces *> parseArgument <* spaces)
		blanks <- many (spaces *> char '_' <* spaces)
		return $ NodeArg (FunctionName fn (length blanks)) binds)
	<?> "argument"

parseArguments :: Parser [Argument]
parseArguments = (spaces *> parseArgument <* spaces) `sepBy` char ','

parseNames :: Parser [Name]
parseNames = (spaces *> parseName <* spaces) `sepBy` char ','

parseVariables :: Parser [Variable]
parseVariables = (spaces *> parseVariable <* spaces) `sepBy` char ','

parseConstructor :: Parser Name
parseConstructor = try (do
	name <- parseName
	guard (isUpper (head (nameIdentifier name)))
	return name)
	<?> "constructor"

parseLiteral :: Parser Literal
parseLiteral =
	LiteralInt . read <$> many1 digit
	<?> "literal"

parsePattern :: Parser Pattern
parsePattern = choice (map try
	[ do
		constructor <- parseConstructor <* spaces
		binds <- many (spaces *> parseVariable <* spaces)
		return $ NodePat (ConstructorName constructor) binds
	, do
		fn <- parseName <* spaces
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
		try (string "@alloc"); spaces
		n <- many1 digit
		return $ Alloc (read n)
	, do
		try (string "@store"); spaces
		parens $ do
			constructor <- parseConstructor <* spaces
			args <- parseArguments
			return $ Store (ConstructorName constructor) args
		  <|> do
		  	fn <- parseName <* spaces
		  	args <- parseArguments
		  	blanks <- many (spaces *> char '_' <* spaces)
		  	return $ Store (FunctionName fn (length blanks)) args
	, do
		try (string "@fetch"); spaces
		ptr <- parseVariable <* spaces
		return $ Fetch ptr
	, do
		try (string "@print"); spaces
		var <- parseVariable <* spaces
		return $ Print var
	, do
		fn <- parseName <* spaces
		args <- parens parseArguments
		return $ Application fn args
	, do
		try (string "@withExceptionHandler"); spaces
		exh <- parseName <* spaces
		exhArgs <- parens parseArguments <* spaces
		fn <- parseName <* spaces
		args <- parens parseArguments
		return $ WithExceptionHandler exh exhArgs fn args
	, do
		try (string "@add"); spaces
		lhs <- parseArgument <* spaces
		rhs <- parseArgument <* spaces
		return $ Add lhs rhs
	]

parseExpression :: Parser Expression
parseExpression = spaces *> choice (map try
	[ do
		try (string "@return"); spaces
		args <- parens parseArguments; spaces
		return $ Return args
	, do
		try (string "case"); spaces
		scrut <- parseVariable <* spaces
		string "of"; spaces
		alts <- (spaces *> parseAlternative <* spaces) `sepBy` char '|'
		return $ Case scrut alts Nothing
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
		e <- parseArgument
		return $ Throw e
	, do
		try (string "@exit"); spaces
		return $ Exit
	, do
		try (string "@tail"); space
		fn <- parseName <* spaces
		args <- parens parseArguments
		return $ TailCall fn args
	]) <?> "expression"

parseModule :: Parser Module
parseModule = do
	fns <- many parseFunction
	eof
	return $ Module [] fns

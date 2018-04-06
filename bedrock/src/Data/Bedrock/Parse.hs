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
stringLiteral, comma, dot :: Parser String
commaSep, commaSep1 :: Parser a -> Parser [a]

identifier = P.identifier lexer
parens     = P.parens lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
symbol     = P.symbol lexer
comma      = P.comma lexer
dot        = P.dot lexer
integer    = P.integer lexer
stringLiteral = P.stringLiteral lexer
commaSep   = P.commaSep lexer
commaSep1  = P.commaSep1 lexer
natural    = P.natural lexer

parseModulePrefix :: Parser String
parseModulePrefix = try $ do
  prefix <- identifier
  guard (isUpper (head prefix))
  return prefix

parseName :: Parser Name
parseName = do
  identifiers <- identifier `sepBy` dot
  n <- do
    symbol "^"
    fromIntegral <$> natural
    <|> pure 0
  return $ Name (init identifiers) (last identifiers) n

parseType :: Parser Type
parseType = choice
  [ symbol "*" >> return NodePtr
  , try $ do
      symbol "%"
      n <- natural
      symbol "%"
      return $ StaticNode (fromIntegral n)
  , symbol "%" >> return Node
  , symbol "#" >> return IWord
  , symbol "@" >> return FramePtr
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
  name <- parseConstructor
  args <- many parseType
  return $ NodeDefinition name args

parseAttribute :: Parser Attribute
parseAttribute = choice
  [ reserved "NoCPS" >> return NoCPS ]

parseFunction :: Parser Function
parseFunction = do
  attributes <- many parseAttribute
  retTypes <- many parseType
  name <- parseName
  args <- parens (commaSep parseVariable)
  body <- parseBlock
  return (Function name attributes args retTypes body)

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

parseNode :: Parser (NodeName, [Variable])
parseNode = choice
  [ do
    constructor <- parseConstructor
    binds <- many parseVariable
    blanks <- many (symbol "_")
    return (ConstructorName constructor (length blanks), binds)
  , do
    fn <- parseName
    binds <- many parseVariable
    blanks <- many (symbol "_")
    return (FunctionName fn (length blanks), binds)
  ]

parsePattern :: Parser Pattern
parsePattern = choice
  [ do
    (name, binds) <- parseNode
    return $ NodePat name binds
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
    reserved "@cast"
    var <- parseVariable
    return $ TypeCast var
  , do
    reserved "@literal"
    Literal <$> parseLiteral
  , do
    reserved "@node"
    (name, binds) <- parens parseNode
    return $ MkNode name binds
  , do
    reserved "@alloc"
    n <- natural
    return $ Alloc (fromIntegral n)
  , do
    reserved "@store"
    parens $ do
      constructor <- parseConstructor
      args <- many parseVariable
      blanks <- many (symbol "_")
      return $ Store (ConstructorName constructor (length blanks)) args
      <|> do
        fn <- parseName
        args <- many parseVariable
        blanks <- many (symbol "_")
        return $ Store (FunctionName fn (length blanks)) args
  , do
    reserved "@fetch"
    ptr <- parseVariable
    let attrs = MemAttributes False Nothing
    return $ Fetch attrs ptr
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
  P.whiteSpace lexer
  topLevel <- many parseTopLevel
  return Module
    { modForeigns = [ f | TopForeign f <- topLevel ]
    , nodes = [ n | TopNode n <- topLevel ]
    , entryPoint = fromMaybe noEntryPoint $ listToMaybe
      [ e | TopEntryPoint e <- topLevel ]
    , functions = [ f | TopFunction f <- topLevel ]
    , modNamespace = AvailableNamespace 0 0 0 0
    }
  where
    noEntryPoint = error "No entrypoint defined."

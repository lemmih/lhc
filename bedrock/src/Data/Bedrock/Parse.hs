module Data.Bedrock.Parse where

import           Control.Monad                          (guard)
import           Data.Char
import           Data.Maybe
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Language as P
import qualified Text.ParserCombinators.Parsec.Token    as P

import           Data.Bedrock
import qualified LLVM.AST            as LLVM (Type (..))
import qualified LLVM.AST.Type       as LLVM

-------------------------------------------------------------------------------
-- Parsing

bedrockDef :: P.LanguageDef ()
bedrockDef = P.emptyDef
  { P.commentLine = ";" -- similar to LLVM IR
  , P.caseSensitive = True
  , P.reservedNames = ["DEFAULT", "entrypoint"]
      -- [ "node", "foreign", "entrypoint", "case", "of" ]
  , P.reservedOpNames = ["=","→"]
  -- , P.identStart = P.identStart P.emptyDef <|> char '@'
  }

lexer :: P.TokenParser ()
lexer = P.makeTokenParser bedrockDef

natural, integer :: Parser Integer
identifier :: Parser String
symbol :: String -> Parser String
parens, brackets, braces :: Parser a -> Parser a
reservedOp, reserved :: String -> Parser ()
stringLiteral, comma, dot :: Parser String
commaSep, commaSep1 :: Parser a -> Parser [a]

identifier = P.identifier lexer
parens     = P.parens lexer
brackets   = P.brackets lexer
braces     = P.braces lexer
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
  identifiers <- identifier `sepBy1` dot
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
  , try $ do
      symbol "@"
      return $ FramePtr
  , Primitive <$> parseLLVMType ]
  <?> "type"

-- i8 i32 i64 void i64*
parseLLVMType :: Parser LLVM.Type
parseLLVMType = withPtrs (try fnType <|> base)
  where
    fnType = do
      ret <- withPtrs base
      args <- parens (parseLLVMType `sepBy` comma)
      return $ LLVM.FunctionType ret args False
    withPtrs p = do
      a <- p
      ptrs <- many (symbol "*")
      return $ foldl (\b _ -> LLVM.ptr b) a ptrs
    base = choice
      [ reserved "i8" >> return LLVM.i8
      , reserved "i16" >> return LLVM.i16
      , reserved "i32" >> return LLVM.i32
      , reserved "i64" >> return LLVM.i64
      , reserved "void" >> return LLVM.VoidType ]

parseVariable :: Parser Variable
parseVariable = do
  ty <- parseType
  case ty of
    Primitive{} -> char '|' *> return ()
    _           -> return ()
  name <- parseName
  return Variable{ variableName = name, variableType = ty }

parseNodeDefinition :: Parser NodeDefinition
parseNodeDefinition = do
  reserved "node"
  name <- parseConstructor
  args <- parens (commaSep parseType)
  return $ NodeDefinition name args

parseAttribute :: Parser Attribute
parseAttribute = choice
  [ reserved "NoCPS" >> return NoCPS
  , reserved "Internal" >> return Internal ]

parseFunction :: Parser Function
parseFunction = do
  attributes <- commaSep parseAttribute
  retTypes <- commaSep parseType
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
  reservedOp "→"
  expression <- parseBlock
  return $ Alternative pat expression

{-
| PString String
| PName Name
| PNodeName NodeName
| PVariable Variable
| PVariables [Variable]
-}
parseParameter :: Parser Parameter
parseParameter = choice
  [ PInt . fromIntegral <$> integer
  , PString <$> stringLiteral
  , do reserved "node"
       PNodeName . fst <$> parseNode
  , PVariable <$> parseVariable
  , PVariables <$> brackets (commaSep parseVariable)
  , PName <$> parseName
  ]

parseExpression :: Parser Expression
parseExpression = choice
  [ do
    reserved "@literal"
    Literal <$> parseLiteral
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
    fn <- parseName
    args <- parens (commaSep parseVariable)
    return $ Application fn args
  , do
    symbol "@"
    fn <- identifier
    args <- parens (commaSep parseParameter)
    return $ Builtin fn args
  ]

parseBlock :: Parser Block
parseBlock = choice (
  [ do
    reserved "@return"
    args <- parens (commaSep parseVariable)
    return $ Return args
  , try $ do
    pat <- parsePattern
    reserved "← "
    scrut <- parseVariable
    rest <- parseBlock
    return $ Case scrut Nothing [Alternative pat rest]
  , do
    reserved "case"
    scrut <- parseVariable
    reserved "of"
    braces $ do
      alts <- many parseAlternative
      mbDef <- option Nothing $ do
        reserved "DEFAULT"
        reserved "→"
        Just <$> parseBlock
      return $ Case scrut mbDef alts
  , do
    reserved "@raise"
    e <- parseVariable
    return $ Raise e
  , do
    reserved "@exit"
    return $ Exit
  , do
    reserved "@tail"
    fn <- parseName
    args <- parens (commaSep parseVariable)
    return $ TailCall fn args
  , do
    reserved "@panic"
    Panic <$> stringLiteral
  , do
    reserved "@invoke"
    fn <- parseVariable
    args <- parens (commaSep parseVariable)
    return $ Invoke fn args
  , try $ do
    names <- commaSep1 parseVariable
    symbol "="
    simple <- parseExpression
    rest <- parseBlock
    return $ Bind names simple rest
  , do
    simple <- parseExpression
    rest <- parseBlock
    return $ Bind [] simple rest
  ])

parseEntryPoint :: Parser Name
parseEntryPoint = do
  reserved "entrypoint"
  symbol ":"
  parseName

parseForeignName :: Parser String
parseForeignName = identifier

-- foreign [ret] name([args])
parseForeign :: Parser Foreign
parseForeign = do
  reserved "foreign"
  ret <- parseLLVMType
  name <- parseForeignName
  args <- parens $ commaSep parseLLVMType
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
    , modLayouts = []
    , entryPoint = fromMaybe noEntryPoint $ listToMaybe
      [ e | TopEntryPoint e <- topLevel ]
    , functions = [ f | TopFunction f <- topLevel ]
    , modNamespace = AvailableNamespace 0 0 0 0
    }
  where
    noEntryPoint = error "No entrypoint defined."

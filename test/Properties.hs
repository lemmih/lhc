module Properties (parsePrettyPrintProps) where

import           Data.Bedrock.Parse
import           Data.Bedrock.PrettyPrint
import           Test.Hspec
import           Test.QuickCheck
import           Text.ParserCombinators.Parsec (Parser, runParser, spaces)
import           Text.PrettyPrint.ANSI.Leijen  (plain)

parseTest :: (Eq a, Pretty a) => Parser a -> a -> Bool
parseTest p x = runParser (spaces >> p) () "test input" (show (plain $ pretty x)) == Right x

parsePrettyPrintProps :: Spec
parsePrettyPrintProps =
  describe "Parse/PrettyPrint" $ do
    it "Name"     $ property $ parseTest parseName
    it "Type"     $ property $ parseTest parseType
    it "Variable" $ property $ parseTest parseVariable
    it "Foreign"  $ property $ parseTest parseForeign
    it "NodeDefinition" $ property $ parseTest parseNodeDefinition
    it "Literal" $ property $ parseTest parseLiteral
    it "Attribute" $ property $ parseTest parseAttribute
    it "Expression" $ property $ parseTest parseExpression
    it "Parameter" $ property $ parseTest parseParameter
    it "Block" $ property $ parseTest parseBlock
    it "Function" $ property $ parseTest parseFunction
    it "Module" $ property $ parseTest parseModule
    return ()

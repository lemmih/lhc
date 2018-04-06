module Properties (parsePrettyPrintProps) where

import           Data.Bedrock.Parse
import           Data.Bedrock.PrettyPrint
import           Test.Hspec
import           Test.QuickCheck
import           Text.ParserCombinators.Parsec (Parser, runParser)
import           Text.PrettyPrint.ANSI.Leijen  (plain)

parseTest :: (Eq a, Pretty a) => Parser a -> a -> Bool
parseTest p x = runParser p () "test input" (show (plain $ pretty x)) == Right x

parsePrettyPrintProps :: Spec
parsePrettyPrintProps = do
  describe "Parse/PrettyPrint" $ do
    it "Name"     $ property $ parseTest parseName
    it "CType"    $ property $ parseTest parseCType
    it "Type"     $ property $ parseTest parseType
    it "Variable" $ property $ parseTest parseVariable
    it "Foreign"  $ property $ parseTest parseForeign
    it "NodeDefinition" $ property $ parseTest parseNodeDefinition
    -- xit "Module" $ property $ parseTest parseModule

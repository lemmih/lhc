module Properties (parsePrettyPrintProps) where

import           Data.Bedrock.Parse
import           Data.Bedrock.PrettyPrint
import           Language.Haskell.TypeCheck.Pretty (pretty)
import           Test.Hspec
import           Test.QuickCheck
import           Text.ParserCombinators.Parsec     (runParser)

parsePrettyPrintProps :: Spec
parsePrettyPrintProps = do
  describe "Parse/PrettyPrint" $ do
    it "Name" $ property $
      \x -> runParser parseName () "test" (show (pretty x)) == Right x
    xit "Type" $ property $
      \x -> runParser parseType () "test" (show (pretty x)) == Right x
    xit "Foreign" $ property $
      \x -> runParser parseForeign () "test" (show (pretty x)) == Right x
    xit "Module" $ property $
      \x -> runParser parseModule () "test" (show (ppModule x)) == Right x

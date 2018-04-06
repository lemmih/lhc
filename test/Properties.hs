module Properties (parsePrettyPrintProps) where

import           Data.Bedrock
import           Data.Bedrock.Parse
import           Data.Bedrock.PrettyPrint
import           Language.Haskell.TypeCheck.Pretty (pretty)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck
import           Text.ParserCombinators.Parsec

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

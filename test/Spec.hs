module Main (main) where

import           Data.Bedrock
import           Data.Bedrock.PrettyPrint
import           Data.Bedrock.Parse
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Text.ParserCombinators.Parsec
import Test.QuickCheck
import           Language.Haskell.TypeCheck.Pretty (pretty)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Bedrock" $ do
    describe "Parse/PrettyPrint" $ do
      it "Name" $ property $
        \x -> runParser parseName () "test" (show (pretty x)) == Right x
      xit "Type" $ property $
        \x -> runParser parseType () "test" (show (pretty x)) == Right x
      xit "Foreign" $ property $
        \x -> runParser parseForeign () "test" (show (pretty x)) == Right x
      xit "Module" $ property $
        \x -> runParser parseModule () "test" (show (ppModule x)) == Right x

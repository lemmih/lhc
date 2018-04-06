module Main (main) where

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit

import           Properties
import           UnitTests

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Bedrock" $
    parsePrettyPrintProps
  describe "LHC" $
    describe "Examples" $ do
      test <- runIO $ unitTestFolder "examples/"
      fromHUnitTest test

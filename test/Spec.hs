module Main (main) where

import           Data.Bedrock
import           Data.Bedrock.PrettyPrint
import           Data.Bedrock.Parse
import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.Hspec.QuickCheck
import           Text.ParserCombinators.Parsec
import Test.QuickCheck
import           Language.Haskell.TypeCheck.Pretty (pretty)
import System.Directory
import System.FilePath
import System.Exit
import System.Process

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
  describe "LHC" $
    describe "Examples" $
      fromHUnitTest $
        TestLabel "HelloWorld.hs" $
        TestCase $ do
          compileHaskell "examples/HelloWorld.hs"
          executeHaskell "examples/HelloWorld.hs" "" "Hello world!\n"

compileHaskell :: FilePath -> IO ()
compileHaskell path = do
  (code, _stdout, _stderr) <- readProcessWithExitCode "stack" ["exec","--","lhc","build",path] ""
  assertExitCode "Compilation failed" code

executeHaskell :: FilePath -> String -> String -> IO ()
executeHaskell path stdin expected_out = do
  lli <- assertMaybe "Couldn't find lli executable" =<< findExecutable "lli"
  (code, stdout, _stderr) <- readProcessWithExitCode lli [replaceExtension path "ll"] stdin
  assertExitCode "Execution failed" code
  assertEqual "Unexpected stdout" expected_out stdout

assertExitCode :: String -> ExitCode -> Assertion
assertExitCode msg ExitSuccess = return ()
assertExitCode msg (ExitFailure code) = assertFailure (msg ++ ", code: " ++ show code)

assertMaybe :: String -> Maybe a -> IO a
assertMaybe _ (Just a)  = return a
assertMaybe msg Nothing = assertFailure msg

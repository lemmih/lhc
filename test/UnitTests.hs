module UnitTests (unitTestFolder) where

import           Control.Monad
import           Data.Maybe
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           Test.HUnit (Test(..), Assertion, assertFailure, assertString, assertEqual)

unitTestFolder :: FilePath -> IO Test
unitTestFolder path = do
  files <- getDirectoryContents path
  tests <- forM files $ \file -> do
    let fullPath = path </> file
    isDir <- doesDirectoryExist fullPath
    case () of
      _ | file=="." || file==".." -> return Nothing
        | isDir -> Just <$> unitTestFolder fullPath
        | takeExtension fullPath == ".hs" -> return $ Just $ TestLabel file $ TestCase $ do
            stdout <- readFileOptional $ addExtension fullPath "stdout"
            stdin  <- readFileOptional $ addExtension fullPath "stdin"
            compileHaskell fullPath
            executeHaskell fullPath stdin stdout
        | otherwise -> return Nothing
  return $ TestList $ catMaybes tests

--------------------------------------------------------------------------------
-- Helpers

compileHaskell :: FilePath -> IO ()
compileHaskell path = do
  (code, _stdout, stderr) <- readProcessWithExitCode "stack" ["exec","--","lhc","build",path] ""
  assertString stderr
  assertExitCode "Compilation failed" code

executeHaskell :: FilePath -> String -> String -> IO ()
executeHaskell path stdin expected_out = do
  lli <- assertMaybe "Couldn't find lli executable" =<< findExecutable "lli"
  (code, stdout, _stderr) <- readProcessWithExitCode lli [replaceExtension path "ll"] stdin
  assertExitCode "Execution failed" code
  assertEqual "Unexpected stdout" expected_out stdout

readFileOptional :: FilePath -> IO String
readFileOptional path = do
  hasFile <- doesFileExist path
  if hasFile then readFile path else return ""

assertExitCode :: String -> ExitCode -> Assertion
assertExitCode _ ExitSuccess = return ()
assertExitCode msg (ExitFailure code) = assertFailure (msg ++ ", code: " ++ show code)

assertMaybe :: String -> Maybe a -> IO a
assertMaybe _ (Just a)  = return a
assertMaybe msg Nothing = assertFailure msg

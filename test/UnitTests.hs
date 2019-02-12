module UnitTests (unitTestFolder) where

import           Control.Monad    (forM)
import           Data.Maybe       (catMaybes)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           Test.HUnit       (Assertion, Test (..), assertEqual,
                                   assertFailure, assertString)

gcStrategies :: [String]
gcStrategies = ["semi"]

unitTestFolder :: FilePath -> IO Test
unitTestFolder path =
  TestList <$> (forM gcStrategies $ \strategy -> do
    test <- unitTestFolderWithGC path strategy
    return $ TestLabel ("gc="++strategy) test)

unitTestFolderWithGC :: FilePath -> String -> IO Test
unitTestFolderWithGC path gcVariant = do
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
            compileHaskell fullPath gcVariant
            executeHaskell fullPath stdin stdout
        | otherwise -> return Nothing
  return $ TestList $ catMaybes tests

--------------------------------------------------------------------------------
-- Helpers

compileHaskell :: FilePath -> String -> IO ()
compileHaskell path gcVariant = do
  (code, _stdout, stderr) <- readProcessWithExitCode "stack" ["exec","--","lhc","build","--gc",gcVariant, path] ""
  assertString stderr
  assertExitCode "Compilation failed" code

findAnExecutable :: [String] -> IO (Maybe FilePath)
findAnExecutable [] = return Nothing
findAnExecutable (x:xs) = do
  mbExec <- findExecutable x
  case mbExec of
    Just exec -> return (Just exec)
    Nothing   -> findAnExecutable xs

executeHaskell :: FilePath -> String -> String -> IO ()
executeHaskell path stdin expected_out = do
  lli <- assertMaybe "Couldn't find lli executable" =<< findAnExecutable ["lli", "lli-7", "lli-6.0"]
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

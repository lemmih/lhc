{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Control.Exception
import           Control.Monad                     (fmap, unless, when)
import           Control.Monad
import qualified Data.ByteString.Lazy              as BL
import           Data.Graph                        (SCC (..), stronglyConnComp)
import           Data.IORef
import           Data.List                         (sort)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import           Language.Haskell.Crux.FromHaskell as Haskell
import           Language.Haskell.Crux.NewTypes    as Haskell
import           Language.Haskell.Crux.Simplify    as Haskell
import           Language.Haskell.Crux.Unique      as Haskell
import           Language.Haskell.Exts             (ImportDecl (..),
                                                    Module (..),
                                                    ModuleHead (..),
                                                    ModuleName (..),
                                                    ParseResult (..),
                                                    SrcSpanInfo (..), parseFile)
import           Language.Haskell.Scope            hiding (Interface)
import           Language.Haskell.TypeCheck
import           Language.Haskell.TypeCheck.Pretty (pretty)
import           System.Environment                (getArgs)
import           System.Directory                  (doesFileExist)
import           System.Exit                       (exitFailure, exitSuccess)
import           System.FilePath                   (replaceExtension,
                                                    takeBaseName)
import           System.IO                         (hPutStrLn, stderr)

import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.Golden

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      exist <- doesFileExist path
      when exist $ do
        info <- desugar [path]
        case info of
          Left err -> do
            putStr err
            hPutStrLn stderr ""
            exitFailure
          Right msg -> do
            putStr msg
            exitSuccess
    _ -> return ()
  goldenFiles <- sort <$> findByExtension [".stdout"] "tests"
  defaultMain $ testGroup "Tests"
    [ (if testName `elem` ignoreList
        then ignoreTest
        else id)
      (goldenVsText testName goldenFile (desugar' testFile))
    | goldenFile <- goldenFiles
    , let testFile = replaceExtension goldenFile "hs"
    , let testName = takeBaseName goldenFile
    ]
  where
    ignoreList = []

desugar' :: FilePath -> IO String
desugar' path = fmap (either id id) (desugar [path])

desugar :: [FilePath] -> IO (Either String String)
desugar files = handleErrors $ do
  ms <- forM files doParseFile
  let graph =
        [ (m, self, imports)
        | m <- ms
        , let (self, imports) = moduleDependencies m ]
      scc = stronglyConnComp graph
  resolveEnvRef <- newIORef emptyResolveEnv
  tiEnvRef <- newIORef emptyTcEnv
  last <$> forM scc (\case
    AcyclicSCC m -> do
      resolveEnv <- readIORef resolveEnvRef
      tiEnv <- readIORef tiEnvRef
      let (resolveEnv', errs, m') = resolve resolveEnv m
      unless (null errs) $ error (unlines $ map show errs)
      case typecheck tiEnv m' of
        Left err -> error (show err)
        Right (typedModule, tiEnv') -> do
          writeIORef resolveEnvRef resolveEnv'
          writeIORef tiEnvRef tiEnv'
          let core =
                Haskell.unique $ Haskell.simplify $ Haskell.simplify $
                Haskell.lowerNewTypes $
                Haskell.convert tiEnv' typedModule
          return $ show (pretty core) ++ "\n"
    CyclicSCC{} -> error "Recursive modules not handled yet.")




handleErrors :: IO String -> IO (Either String String)
handleErrors action = toMsg <$> try action
  where
    toMsg (Left (ErrorCallWithLocation msg loc)) = Left (unlines [msg,loc])
    toMsg (Right val)                            = Right val

doParseFile :: FilePath -> IO (Module SrcSpanInfo)
doParseFile file = do
  ret <- parseFile file
  case ret of
    ParseOk m -> pure m
    ParseFailed src msg ->
      error (show src ++ ":" ++ msg)

moduleDependencies :: Module a -> (String, [String])
moduleDependencies (Module _ mbHead _pragma imports _decls) =
  (case mbHead of
    Nothing                                                -> "Main"
    Just (ModuleHead _ (ModuleName _ name) _warn _exports) -> name
  , [ modName
    | importDecl <- imports
    , let ModuleName _ modName = importModule importDecl ])
moduleDependencies _ = error "Main: moduleDependencies: undefined"


goldenVsText :: TestName -> FilePath -> IO String -> TestTree
goldenVsText name path gen =
    goldenVsStringDiff name (\ref new -> ["diff", ref, new]) path gen'
  where
    gen' = BL.fromStrict . T.encodeUtf8 . T.pack <$> gen

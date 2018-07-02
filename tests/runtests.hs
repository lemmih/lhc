{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Control.Monad                     (fmap, mplus, unless, when)
import           Language.Haskell.Crux.FromHaskell as Haskell
import           Language.Haskell.Crux.NewTypes    as Haskell
import           Language.Haskell.Crux.Simplify    as Haskell
import           Language.Haskell.Exts             (ImportDecl (..),
                                                    Module (..),
                                                    ModuleHead (..),
                                                    ModuleName (..),
                                                    ParseResult (..),
                                                    SrcSpanInfo (..), parseFile)
import           Language.Haskell.Scope            hiding (Interface)
import           Language.Haskell.TypeCheck
import           Language.Haskell.TypeCheck.Pretty (pretty)

import           Control.Exception
import           Control.Monad
import           Data.Graph                        (SCC (..), stronglyConnComp)
import           Data.IORef
import           System.Environment                (getArgs)
import           System.Exit                       (exitFailure, exitSuccess)
import           System.FilePath                   (replaceExtension,
                                                    takeBaseName, (<.>), (</>))
import           System.IO                         (hPutStrLn, stderr)
import           Test.Framework                    (Test, defaultMain,
                                                    testGroup)
import           Test.Framework.Providers.HUnit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> defaultMain unitTests
    files -> do
      info <- desugar files
      case info of
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right msg -> do
          putStr msg
          exitSuccess

unitTests :: [Test]
unitTests =
  [ sugarTest "Prim"
  , testGroup "Haskell2010"
    [
    ]
  , testGroup "Extensions"
    [ ]
  , testGroup "Known issues"
    [ ]
  ]

sugarTest :: String -> Test
sugarTest name = testCase (takeBaseName name) $ do
  let testFile = "tests" </> name <.> "hs"
  expectedOutput <- readFile (replaceExtension testFile "stdout") `mplus` return ""
  output <- either id id `fmap` desugar [testFile]
  let isFailure = expectedOutput /= output
  when isFailure $ fail "Diff Error"



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
          let core = Haskell.simplify $ Haskell.simplify $ Haskell.lowerNewTypes $
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

{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Data.Graph                         (SCC (..), stronglyConnComp)
import           Data.Tagged
import           Language.Haskell.Exts
import           Language.Haskell.TypeCheck.Pretty  (displayIO, pretty,
                                                     renderPretty)
import           System.Exit
import           System.FilePath
import           System.IO

import           Language.Haskell.Scope             hiding (Interface)
import           Language.Haskell.TypeCheck

import qualified Compiler.CoreToBedrock             as Core
import           Control.Monad
import qualified Data.Bedrock.Compile               as Bedrock
import           Data.Bedrock.Storage.Fixed
import           Data.Bedrock.Storage.SemiSpace
import           Data.IORef
import           Data.List                          (intercalate)
import           Data.Monoid                        (mconcat, (<>))
import qualified Distribution.ModuleName            as Dist
import           Interpret
import qualified Language.Haskell.Crux              as Core
import qualified Language.Haskell.Crux.DCE          as Core
import qualified Language.Haskell.Crux.FromHaskell  as Haskell
import           Language.Haskell.Crux.Interface
import qualified Language.Haskell.Crux.NewTypes     as NewType
import qualified Language.Haskell.Crux.SimpleEta    as Core
import qualified Language.Haskell.Crux.SimpleInline as Core
import qualified Language.Haskell.Crux.Simplify     as Core
import qualified Language.Haskell.Crux.Unique       as Core
import           Options.Applicative
import           RTS                                (linkRTS)
import           System.Directory

import           Codec.Serialise
import qualified Distribution.HaskellSuite.Compiler as Compiler
import           Distribution.HaskellSuite.Packages
import           Distribution.InstalledPackageInfo  (ExposedModule (..),
                                                     InstalledPackageInfo,
                                                     exposedModules,
                                                     libraryDirs)
import           Distribution.Version


import           Paths_lhc

main :: IO ()
main = Compiler.customMain customCommands lhcCompiler

customCommands :: Parser (IO ())
customCommands = hsubparser (buildCommand <> interpretCommand)
  where
    interpretCommand = command "interpret" (info int idm)
    int =
      runInterpreter
      <$> switch (long "verbose")
      <*> argument str (metavar "MODULE")
    buildCommand = command "build" (info build idm)
    build =
        compileExecutable
        <$> switch (long "verbose")
        <*> switch (long "keep-intermediate-files" <> help "Write bedrock files to disk")
        <*> strOption (long "gc" <> metavar "strategy" <> help "GC strategy" <> value "fixed" <> showDefault)
        <*> argument str (metavar "MODULE")

data LHC
instance IsDBName LHC where
    getDBName = Tagged "lhc"

lhcCompiler :: Compiler.Simple (StandardDB LHC)
lhcCompiler =
    Compiler.simple
        "lhc"
        (mkVersion' version)
        [Haskell2010]
        []
        compileLibrary
        ["hi", "core", "core.pretty"]


moduleFile :: Module Origin -> FilePath
moduleFile m =
    case m of
        Module _ (Just (ModuleHead _ (ModuleName _ modName) _ _)) _ _ _ ->
            replace '.' pathSeparator modName
        _ -> error "Urk"
  where
    replace a b lst = [ if c == a then b else c | c <- lst ]

moduleDependencies :: Module a -> (String, [String])
moduleDependencies (Module _ mbHead _pragma imports _decls) =
  (case mbHead of
    Nothing                                                -> "Main"
    Just (ModuleHead _ (ModuleName _ name) _warn _exports) -> name
  , [ modName
    | importDecl <- imports
    , let ModuleName _ modName = importModule importDecl ])
moduleDependencies _ = error "Main: moduleDependencies: undefined"
{-
parse files
find module dependencies
name resolution by scc group
type check by scc group
save .hi, .code, and .pretty files
-}
compileLibrary :: Compiler.CompileFn
compileLibrary buildDir mbLang exts cppOpts pkgName pkgdbs deps files = do
    ms <- forM files $ \file -> do
            ret <- parseFile file
            case ret of
              ParseOk m -> return m
              ParseFailed src msg -> do
                putStrLn (show src ++ ": " ++ msg)
                exitWith (ExitFailure 1)
    let graph =
          [ (m, self, imports)
          | m <- ms
          , let (self, imports) = moduleDependencies m ]
        scc = stronglyConnComp graph
    resolveEnvRef <- newIORef emptyResolveEnv
    tiEnvRef <- newIORef emptyTcEnv
    forM_ scc $ \case
      AcyclicSCC m -> do
        resolveEnv <- readIORef resolveEnvRef
        tiEnv <- readIORef tiEnvRef
        putStrLn "Origin analysis..."
        let (resolveEnv', errs, m') = resolve resolveEnv m
            Just scopeIface = lookupInterface (getModuleName m) resolveEnv'
        unless (null errs) $ do
          mapM_ print errs
          exitWith (ExitFailure 1)
        putStrLn "Typechecking..."
        case typecheck tiEnv m' of
          Left err -> error (show err)
          Right (typedModule, tiEnv') -> do
            let iface = mkInterface scopeIface tiEnv'
                ifaceFile = buildDir </> moduleFile m' <.> "hi"
            createDirectoryIfMissing True (buildDir </> moduleFile m')
            writeInterface ifaceFile iface
            putStrLn "Converting to core..."
            let core = Haskell.convert tiEnv' typedModule
                coreFile = buildDir </> moduleFile m' <.> "core"
                complete = Core.simplify $ Core.simplify $ NewType.lowerNewTypes $ Core.simplify $ Core.simplify core
                (_,etaAbs) = Core.simpleEta Core.emptySimpleEtaAnnotation complete
            writeFileSerialise coreFile etaAbs
            writeFile (coreFile <.> "pretty") (show $ pretty etaAbs)
            writeIORef resolveEnvRef resolveEnv'
            writeIORef tiEnvRef tiEnv'
      CyclicSCC{} -> error "Recursive modules not handled yet."

loadLibrary :: InstalledPackageInfo -> IO [(String, (Interface, Core.Module))]
loadLibrary pkgInfo =
  forM (exposedModules pkgInfo) $ \exposedModule -> do
    Just hiFile <- findFile (libraryDirs pkgInfo) (Dist.toFilePath (exposedName exposedModule) <.> "hi")
    Just coreFile <- findFile (libraryDirs pkgInfo) (Dist.toFilePath (exposedName exposedModule) <.> "core")
    iface <- readInterface hiFile
    core <- readFileDeserialise coreFile
    return (intercalate "." (Dist.components $ exposedName exposedModule), (iface, core))

-- Load dependencies interface files
-- convert to scope interfaces
-- do scoping analysis
-- convert to type interfaces
-- do type checking
-- convert to core
-- merge with library core files
-- convert to bedrock
compileExecutable :: Bool -> Bool -> String -> FilePath -> IO ()
compileExecutable verbose keepIntermediateFiles gcStrategy file = do
  -- putStrLn $ "Loading deps: " ++ show deps
  db <- userDB
  pkgs <- readPackageDB Don'tInitDB (db :: StandardDB LHC)
  -- pkgs <- readPackagesInfo
  --             (Proxy :: Proxy (StandardDB LHC))
  --             [GlobalPackageDB, UserPackageDB] deps
  ifaces <- concat <$> mapM loadLibrary pkgs
  let scope =
          [ (modName, toScopeInterface iface)
          | (modName, (iface, _core)) <- ifaces ]
      scopeEnv = fromInterfaces scope

  when verbose $ putStrLn "Parsing file..."
  ParseOk m <- parseFile file
  when verbose $ putStrLn "Origin analysis..."
  let (resolveEnv, errs, m') = resolve scopeEnv m
      Just _scopeIface = lookupInterface (getModuleName m) resolveEnv
  unless (null errs) $ do
    mapM_ print errs
    exitWith (ExitFailure 1)
  when verbose $ putStrLn "Typechecking..."
  let env = addAllToTcEnv (map (fst . snd) ifaces) emptyTcEnv
  let Right (typedModule, env') = typecheck env m'
  when verbose $ putStrLn "Converting to core..."
  let core = Haskell.convert env' typedModule
      libraryCore = mconcat (map (snd . snd) ifaces)
      entrypoint = Core.Name ["Main"] "entrypoint" 0
      base = Core.deadCodeElimination entrypoint $
             NewType.lowerNewTypes $ mappend libraryCore core
      complete =
          Core.deadCodeElimination entrypoint $
          Core.unique $
          Core.simplify $ Core.simplify $ Core.simplify $ Core.simpleInline $ Core.unique $
          Core.simplify $ Core.simplify $ Core.simplify $ Core.simpleInline $ Core.unique $
          Core.simplify $ Core.simplify $ Core.simplify $ Core.simpleInline $ Core.unique $
          snd $ Core.simpleEta Core.emptySimpleEtaAnnotation $
          Core.simplify $ Core.simplify $ Core.simplify $
          snd $ Core.simpleEta Core.emptySimpleEtaAnnotation
          base
  -- print (pretty complete)
  when verbose $ do
    displayIO stdout (renderPretty 1 100 (pretty base))
    displayIO stdout (renderPretty 1 100 (pretty complete))

  let bedrock = Core.convert complete
  -- print (ppModule bedrock)
  let gc = case gcStrategy of
             ""      -> fixedGC
             "fixed" -> fixedGC
             "semi"  -> semiSpaceGC
             _       -> error "Unknown strategy. Options are: fixed, semi, cached"
  let target = replaceExtension file "ll"
  Bedrock.compileModule keepIntermediateFiles verbose gc bedrock target
  linkRTS target

runInterpreter :: Bool -> FilePath -> IO ()
runInterpreter verbose file = do
  -- putStrLn $ "Loading deps: " ++ show deps
  db <- userDB
  pkgs <- readPackageDB Don'tInitDB (db :: StandardDB LHC)
  -- pkgs <- readPackagesInfo
  --             (Proxy :: Proxy (StandardDB LHC))
  --             [GlobalPackageDB, UserPackageDB] deps
  ifaces <- concat <$> mapM loadLibrary pkgs
  let scope =
          [ (modName, toScopeInterface iface)
          | (modName, (iface, _core)) <- ifaces ]
      scopeEnv = fromInterfaces scope

  when verbose $ putStrLn "Parsing file..."
  ParseOk m <- parseFile file
  when verbose $ putStrLn "Origin analysis..."
  let (resolveEnv, errs, m') = resolve scopeEnv m
      Just _scopeIface = lookupInterface (getModuleName m) resolveEnv
  unless (null errs) $ do
    mapM_ print errs
    exitWith (ExitFailure 1)
  when verbose $ putStrLn "Typechecking..."
  let env = addAllToTcEnv (map (fst . snd) ifaces) emptyTcEnv
  let Right (typedModule, env') = typecheck env m'
  when verbose $ putStrLn "Converting to core..."
  let core = Haskell.convert env' typedModule
      libraryCore = mconcat (map (snd . snd) ifaces)
      entrypoint = Core.Name ["Main"] "entrypoint" 0
      base = Core.deadCodeElimination entrypoint $
             NewType.lowerNewTypes $ mappend libraryCore core
      complete =
          Core.deadCodeElimination entrypoint $
          Core.unique $
          Core.simplify $ Core.simplify $ Core.simplify $ Core.simpleInline $ Core.unique $
          Core.simplify $ Core.simplify $ Core.simplify $ Core.simpleInline $ Core.unique $
          Core.simplify $ Core.simplify $ Core.simplify $ Core.simpleInline $ Core.unique $
          snd $ Core.simpleEta Core.emptySimpleEtaAnnotation $
          Core.simplify $ Core.simplify $ Core.simplify $
          snd $ Core.simpleEta Core.emptySimpleEtaAnnotation
          base
  when verbose $ do
    displayIO stdout (renderPretty 1 100 (pretty complete))
    putStr "\n"

  interpret complete

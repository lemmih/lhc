module Main where

import           Data.Graph                         (SCC (..), flattenSCC,
                                                     stronglyConnComp)
import           Data.Tagged
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.TypeCheck.Pretty  (displayIO, pretty,
                                                     renderPretty)
import           System.Exit
import           System.FilePath
import           System.IO                          (stdout)

import           Language.Haskell.TypeCheck.Infer
import           Language.Haskell.TypeCheck.Monad
--import Language.Haskell.TypeCheck.Types
import           Language.Haskell.Scope             hiding (Interface)

import qualified Compiler.Core                      as Core
import qualified Compiler.Core.DCE                  as Core
import qualified Compiler.Core.NewType              as NewType
import qualified Compiler.Core.SimpleEta            as Core
import qualified Compiler.Core.Unique               as Core
import qualified Compiler.Core.Simplify             as Core
import qualified Compiler.Core.SimpleInline         as Core
import qualified Compiler.CoreToBedrock             as Core
import qualified Compiler.HaskellToCore             as Haskell
import           Compiler.Interface
import           Control.Monad
import           Data.Bedrock                       (Name (..))
import qualified Data.Bedrock.Compile               as Bedrock
import           Data.Bedrock.PrettyPrint
import           Data.Binary
import           Data.IORef
import           Data.List                          (intercalate)
import           Data.Monoid                        (mconcat)
import           Data.Proxy
import qualified Distribution.ModuleName            as Dist
import           Options.Applicative
import           System.Directory

import qualified Distribution.HaskellSuite.Compiler as Compiler
import           Distribution.HaskellSuite.Packages
import           Distribution.InstalledPackageInfo  (ExposedModule (..),
                                                     InstalledPackageInfo,
                                                     InstalledPackageInfo_ (..))
import           Distribution.Package
import           Distribution.Simple.Compiler

import           Paths_lhc

main :: IO ()
main = Compiler.customMain customCommands lhcCompiler

customCommands = hsubparser (buildCommand)
  where
    buildCommand = command "build" (info build idm)
    build =
        compileExecutable
        <$> (many $ InstalledPackageId <$> strOption (long "package-id"))
        <*> (argument str (metavar "MODULE"))

data LHC
instance IsDBName LHC where
    getDBName = Tagged "lhc"

lhcCompiler :: Compiler.Simple (StandardDB LHC)
lhcCompiler =
    Compiler.simple
        "lhc"
        version
        [Haskell2010]
        []
        compileLibrary
        ["hi", "core", "core.pretty"]


moduleFile :: Module Origin -> FilePath
moduleFile m =
    case m of
        Module _ (Just (ModuleHead _ (ModuleName _ modName) _ _)) _ _ _ ->
            replace '.' pathSeparator modName
  where
    replace a b lst = [ if c == a then b else c | c <- lst ]

moduleDependencies :: Module a -> (String, [String])
moduleDependencies (Module _ mbHead _pragma imports _decls) =
  (case mbHead of
    Nothing -> "Main"
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
    forM_ scc $ \group -> do
      case group of
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
          tiEnv' <- runTI tiEnv (tiModule m')
          let iface = mkInterface scopeIface tiEnv'
              ifaceFile = buildDir </> moduleFile m' <.> "hi"
          writeInterface ifaceFile iface
          putStrLn "Converting to core..."
          let core = Haskell.convert tiEnv' m'
              coreFile = buildDir </> moduleFile m' <.> "core"
              complete = Core.simplify $ NewType.lower $ Core.simplify $ Core.simplify $ core
              (_,etaAbs) = Core.simpleEta Core.emptySimpleEtaAnnotation complete
          -- print (pretty complete)
          displayIO stdout (renderPretty 1 120 (pretty etaAbs))
          encodeFile coreFile etaAbs
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
        core <- decodeFile coreFile
        return (intercalate "." (Dist.components $ exposedName exposedModule), (iface, core))

-- Load dependencies interface files
-- convert to scope interfaces
-- do scoping analysis
-- convert to type interfaces
-- do type checking
-- convert to core
-- merge with library core files
-- convert to bedrock
compileExecutable :: [InstalledPackageId] -> FilePath -> IO ()
compileExecutable deps file = do
    putStrLn $ "Loading deps: " ++ show deps
    pkgs <- readPackagesInfo
                (Proxy :: Proxy (StandardDB LHC))
                [GlobalPackageDB, UserPackageDB] deps
    ifaces <- concat <$> mapM loadLibrary pkgs
    let scope =
            [ (modName, toScopeInterface iface)
            | (modName, (iface, _core)) <- ifaces ]
        scopeEnv = fromInterfaces scope

    putStrLn "Parsing file..."
    ParseOk m <- parseFile file
    putStrLn "Origin analysis..."
    let (resolveEnv, errs, m') = resolve scopeEnv m
        Just _scopeIface = lookupInterface (getModuleName m) resolveEnv
    mapM_ print errs
    putStrLn "Typechecking..."
    let env = addAllToTcEnv (map (fst . snd) ifaces) emptyTcEnv
    env <- runTI env (tiModule m')
    putStrLn "Converting to core..."
    let core = Haskell.convert env m'
        libraryCore = mconcat (map (snd . snd) ifaces)
        entrypoint = Name ["Main"] "entrypoint" 0
        complete =
            -- Core.deadCodeElimination entrypoint $
            -- Core.simpleInline $
            -- Core.unique $
            Core.simplify $ Core.simplify $ Core.simplify $
            snd $ Core.simpleEta Core.emptySimpleEtaAnnotation $
            Core.deadCodeElimination entrypoint $
            NewType.lower $ mappend libraryCore core
    -- print (pretty complete)
    displayIO stdout (renderPretty 1 100 (pretty complete))

    let bedrock = Core.convert complete
    print (ppModule bedrock)
    Bedrock.compileModule bedrock file
    return ()


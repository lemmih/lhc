module Main where

import           Data.Tagged
import           System.FilePath
import           Text.PrettyPrint.ANSI.Leijen       (pretty)

import           Language.Haskell.Exts.Annotated

import           Language.Haskell.TypeCheck.Infer
import           Language.Haskell.TypeCheck.Monad
--import Language.Haskell.TypeCheck.Types
import           Language.Haskell.Scope             hiding (Interface)

import qualified Compiler.Core                      as Core
import qualified Compiler.Core.NewType              as NewType
import qualified Compiler.Core.Simplify as Core
import qualified Compiler.CoreToBedrock             as Core
import qualified Compiler.HaskellToCore             as Haskell
import           Compiler.Interface
import qualified Data.Bedrock.Compile               as Bedrock
import           Control.Monad
import           Data.Bedrock.PrettyPrint
import           Data.Binary
import           Data.List                          (intercalate)
import           Data.Monoid                        (mconcat)
import           Data.Proxy
import qualified Distribution.ModuleName            as Dist
import           Options.Applicative
import           System.Directory

import qualified Distribution.HaskellSuite.Compiler as Compiler
import           Distribution.HaskellSuite.Packages
import           Distribution.InstalledPackageInfo  (InstalledPackageInfo,
                                                     InstalledPackageInfo_ (..))
import           Distribution.Package
import           Distribution.Simple.Compiler

import           Paths_lhc

-- testInfer :: IO ()
-- testInfer = do
--     ParseOk m <- parseFile "src/Test.hs"
--     let (resolveEnv, errs, m') = resolve emptyResolveEnv m
--     mapM_ print errs
--     _env <- runTI (tiModule m')
--     return ()

--testCompile :: IO ()
--testCompile = do
--    ParseOk m <- parseFile "src/BedrockIO.hs"
--    let (errs, m') = resolve m
--    mapM_ print errs
--    env <- runTI (tiModule m')
--    let core = Haskell.convert env m'
--    let bedrock = Core.convert core
--    print (ppModule bedrock)
--    --compileWithOpts True True "FromHaskell.rock" bedrock

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

compileLibrary :: Compiler.CompileFn
compileLibrary buildDir mbLang exts cppOpts pkgName pkgdbs deps [file] = do
    putStrLn "Parsing file..."
    ParseOk m <- parseFile file
    putStrLn "Origin analysis..."
    let (resolveEnv, errs, m') = resolve emptyResolveEnv m
        Just scopeIface = lookupInterface (getModuleName m) resolveEnv
    mapM_ print errs
    putStrLn "Typechecking..."
    env <- runTI emptyTcEnv (tiModule m')
    let iface = mkInterface scopeIface env
        ifaceFile = buildDir </> moduleFile m' <.> "hi"
    writeInterface ifaceFile iface
    putStrLn "Converting to core..."
    let core = Haskell.convert env m'
        coreFile = buildDir </> moduleFile m' <.> "core"
    encodeFile coreFile core
    writeFile (coreFile <.> "pretty") (show $ pretty core)
    let bedrock = Core.convert core
    print (ppModule bedrock)
    let bedrockFile = replaceExtension file "bedrock"
    writeFile bedrockFile (show $ ppModule bedrock)
    -- Bedrock.compileModule bedrock file
    return ()

loadLibrary :: InstalledPackageInfo -> IO [(String, (Interface, Core.Module))]
loadLibrary pkgInfo =
    forM (exposedModules pkgInfo) $ \modName -> do
        Just hiFile <- findFile (libraryDirs pkgInfo) (Dist.toFilePath modName <.> "hi")
        Just coreFile <- findFile (libraryDirs pkgInfo) (Dist.toFilePath modName <.> "core")
        iface <- readInterface hiFile
        core <- decodeFile coreFile
        return (intercalate "." (Dist.components modName), (iface, core))

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
    pkgs <- readPackagesInfo (Proxy :: Proxy (StandardDB LHC)) [GlobalPackageDB, UserPackageDB] deps
    ifaces <- concat <$> mapM loadLibrary pkgs
    let scope =
            [ (modName, toScopeInterface iface)
            | (modName, (iface, _core)) <- ifaces ]
        scopeEnv = fromInterfaces scope

    putStrLn "Parsing file..."
    ParseOk m <- parseFile file
    putStrLn "Origin analysis..."
    let (resolveEnv, errs, m') = resolve scopeEnv m
        Just scopeIface = lookupInterface (getModuleName m) resolveEnv
    mapM_ print errs
    putStrLn "Typechecking..."
    let env = addAllToTcEnv (map (fst . snd) ifaces) emptyTcEnv
    env <- runTI env (tiModule m')
    putStrLn "Converting to core..."
    let core = Haskell.convert env m'
        libraryCore = mconcat (map (snd . snd) ifaces)
        complete = Core.simplify $ NewType.lower $ mappend libraryCore core
    print (pretty complete)

    let bedrock = Core.convert complete
    print (ppModule bedrock)
    Bedrock.compileModule bedrock file
    return ()


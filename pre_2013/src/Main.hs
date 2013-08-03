{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}
module Main (main) where

import System.Directory
import System.FilePath
import System.Cmd
import qualified Data.Version as Version
import Paths_lhc
import qualified Data.ByteString.Lazy.Char8 as L
import System.IO
import System.Exit
import qualified Data.Map as Map
import Data.Binary
import Control.Monad

import CompactString
import qualified Language.Core as Core
import Grin.SimpleCore
import Grin.FromCore
import Grin.Pretty
import qualified Grin.SimpleCore.DeadCode as Simple
--import qualified Grin.SimpleCore.Specialize as Spec
import qualified Grin.Optimize.Simple as Simple
--import qualified Grin.Optimize.Case as Case
--import qualified Grin.DeadCode as DeadCode
import qualified Grin.PreciseDeadCode as DeadCode
import qualified Grin.Optimize.Inline as Inline
import qualified Grin.Optimize.CAF as CAF
import qualified Grin.Optimize.CallPattern as CallPattern

--import Grin.Rename
import qualified Grin.HPT as HPT
--import qualified Grin.HPT.GraphSolve as SCC
import qualified Grin.Lowering.Apply as Apply

import qualified Grin.Stage2.FromStage1 as Stage2
import qualified Grin.Stage2.Pretty as Stage2
import qualified Grin.Stage2.Optimize.Simple as Stage2.Simple
import qualified Grin.Stage2.Optimize.Case as Stage2.Case
--import qualified Grin.Stage2.Backend.C as Backend.C
import qualified Grin.Stage2.DeadCode  as Stage2
import qualified Grin.Stage2.Rename    as Stage2

--import qualified Grin.Stage3.Types
import qualified Grin.Stage3.FromStage2 as Stage3
import qualified Grin.Stage3.Pretty as Stage3
import qualified Grin.Stage3.Optimize.Simple as Stage3.Simple
import qualified Grin.Stage3.Optimize.Inline as Stage3
import qualified Grin.Stage3.DeadCode as Stage3
import qualified Grin.Stage3.Backend.C as Stage3.C

import qualified Grin.MinusMinus.Types
import qualified Grin.MinusMinus.Pretty as MinusMinus
import qualified Grin.MinusMinus.Convert as MinusMinus
import qualified Grin.MinusMinus.Backend.C as MinusMinus.C

import Manager

import GhcDriver ( compileHsToCoreFiles, compileCFilesWithGhc )

import Config
import Setup     ( getMode )




defGhcCompileOpts = ["-no-link", "-fext-core", "-O"]

-- Below are language options for Haskell 2010
defGhcLangOpts    = ["-XHaskell2010"]
{-
defGhcLangOpts    = map ("-X"++) [ "PatternGuards"
                                 , "EmptyDataDecls"
                                 , "ForeignFunctionInterface"
                                 , "NoMonomorphismRestriction"
                                 , "NoNPlusKPatterns"]
-}

main :: IO ()
main = do let numeric_ver = Version.showVersion version
              banner = "The LHC Haskell Compiler, v"++numeric_ver++", (C) 2009-2010 David Himmelstrup, Austin Seipp"
          mode <- getMode banner
          case mode of
            Version              -> putStrLn banner
            NumericVersion       -> putStrLn numeric_ver
            SupportedLanguages   -> do system "ghc --supported-languages"; return ()
            InstallLibrary files -> mapM_ installCoreFile files
            Normal cfg files     -> do let hasCfiles = any ((=="c") . takeExtension) files
                                           cfiles = filter ((=="c") . takeExtension) files
                                           copts = defGhcCompileOpts ++ defGhcLangOpts ++ configGhcOptions cfg
                                           dopts = defGhcLangOpts ++ configGhcOptions cfg
                                       if hasCfiles
                                          then compileCFilesWithGhc (configGhcOptions cfg) (configOutputFile cfg) cfiles
                                          else do corefiles <- compileHsToCoreFiles (configBuildLibrary cfg) (configNoLink cfg) copts dopts files
                                                  let realCorefiles = [ configSrcDir cfg </> path | path <- corefiles ]
                                                  unless (configNoLink cfg || configBuildLibrary cfg) $ 
                                                    build cfg realCorefiles

             


installCoreFile :: FilePath -> IO ()
installCoreFile path
    = do inp <- L.readFile path
         hPutStr stderr $ "Parsing " ++ path ++ "..."
         hFlush stdout
         case Core.parseModule "file" inp of
           Left errs -> hPutStrLn stderr "errors: " >> print errs
           Right mod  -> do hPutStrLn stderr " done"
                            dataDir <- getAppUserDataDirectory "lhc"
                            let packagesDir = dataDir </> "packages"
                            let smod = coreToSimpleCore mod
                            createDirectoryIfMissing False (packagesDir </> modulePackage smod)
                            encodeFile (packagesDir </> modulePackage smod </> moduleName smod) smod

-- | Build a set of external core files, by compiling them to C or LLVM. Loads all registered libraries for optimization.
build :: Config -- Build-time options.
      -> [FilePath] -- ^ External core files
      -> IO ()
build _ [] = putStrLn "no input files specified" >> exitWith (ExitFailure 1)
build cfg files@(file:_)
    = do let keepTmpFiles = configKeepTmpFiles cfg

         mods <- mapM parseCore files
         libs <- loadAllLibraries
         let primModule = SimpleModule { modulePackage = "ghczmprim"
                                       , moduleName    = "GHCziPrim"
                                       , moduleNewTypes = Map.empty
                                       , moduleTypes   = [SimpleType (fromString "ghc-prim:GHC.Prim.(# #)") 1 0
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,#)") 2 0
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,#)") 3 0
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,#)") 4 0
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,#)") 5 0
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,#)") 6 0
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,#)") 7 0
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,,#)") 8 0
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,,,#)") 9 0
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,,,,#)") 10 0
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,,,,,#)") 11 0
                                                         ,SimpleType (fromString "ghc-prim:GHC.Prim.(#,,,,,,,,,,,#)") 12 0
                                                         ]
                                       , moduleEnums   = []
                                       , moduleDefs    = [] }
         let allModules = Map.insert (modulePackage primModule, moduleName primModule) primModule $
                          foldr (\mod -> Map.insert (modulePackage mod, moduleName mod) mod) libs mods
                          -- libs
             (newTypes', tdefs', enums', defs') = Simple.removeDeadCode [("main","Main")]  ["main::Main.main"] allModules
             (tdefs, enums, defs) = (tdefs',enums',defs')  --Spec.superSpecialize "main::Main.main" newTypes' tdefs' enums' defs'
             grin = coreToGrin tdefs enums defs
         let target = replaceExtension file "lhc"

         -- TODO: optimizeLvl should reflect the optimization settings here on GRIN, not just C compiled code
         when keepTmpFiles $ outputGrin target "_raw" grin
         first_fixpoint <- transformer (if keepTmpFiles then Just (replaceExtension file "grin") else Nothing)
--                           [ Fixpoint
                             [ Single "Simple " (DeadCode.trimDeadCode .Inline.inlinePass . DeadCode.trimDeadCode . Simple.optimize)
                             , Single "CallPattern" CallPattern.optimize
                             , Single "CAF" CAF.optimize
                             , Single "Simple " (DeadCode.trimDeadCode .Inline.inlinePass . DeadCode.trimDeadCode . Simple.optimize) ]
                           grin
         let applyLowered = Apply.lower first_fixpoint
             hptEnv = HPT.mkEnvironment applyLowered
             iterations = HPT.analyze applyLowered
             hpt = last iterations
             (evalLowered, hpt') = HPT.lower (configOpt cfg) hpt applyLowered
         timeIt "Lowering apply primitives" $ do let !lowered = applyLowered
                                                 when keepTmpFiles $ outputGrin target "_apply" lowered
         --timeIt "Strongly connected equations" $ do let scc = SCC.formatSCC (SCC.sccEquations hptEnv)
         --                                           writeFile (replaceExtension target "scc") scc
         writeFile (replaceExtension target "eqs") (HPT.showEquations hptEnv)
         timeIt "Heap points-to analysis" $ do forM_ iterations $ \_ -> do putStr "."; hFlush stdout
                                               let !lowered = evalLowered
                                               when keepTmpFiles $ outputGrin target "_eval" lowered
         putStrLn $ "HPT fixpoint found in " ++ show (length iterations) ++ " iterations."
         writeFile (replaceExtension target "eqs.solved") (show hpt)

         -- TODO: optimizeLvl should reflect the optimization settings here on GRIN, not just C compiled code
         let stage2_raw = Stage2.convert (configOpt cfg) hpt' evalLowered
         second_fixpoint <- transformer (if keepTmpFiles then Just (replaceExtension file "grin2") else Nothing)
                            [ 
                              Single "Optimize" Stage2.Simple.optimize
                            , Single "Case optimize" Stage2.Case.optimize
                            , Single "Rename" Stage2.rename
                              --, Single "Apply rewrite rules" Stage2.Case.applyRewriteRules
                            , Single "Inline" (Stage2.trimDeadCode . Stage2.Case.inlinePass)
                              --, Single "Apply rewrite rules" Stage2.Case.applyRewriteRules
                              --, Single "Remove dead code" Stage2.trimDeadCode
                            , Single "Optimize" (Stage2.Simple.optimize . Stage2.trimDeadCode)
                            ]
                            stage2_raw
         let stage2_out = second_fixpoint
         when keepTmpFiles $ outputGrin2 target "" stage2_out

         let stage3_raw = Stage3.convert stage2_out
         when keepTmpFiles $ outputGrin3 target "_raw" stage3_raw
         third_fixpoint <- transformer (if keepTmpFiles then Just (replaceExtension file "grin3") else Nothing)
                           [ Single "Optimize" Stage3.Simple.optimize
                           , Single "Inline" Stage3.inline
                           , Single "Remove dead code" Stage3.trimDeadCode
                           , Single "Optimize" Stage3.Simple.optimize
                           ] stage3_raw
         when keepTmpFiles $ outputGrin3 target "" third_fixpoint

         let mmGrin = MinusMinus.convert third_fixpoint
         when keepTmpFiles $ outputGrinMM target "" mmGrin

         if not (configUseGrinMM cfg)
            then timeIt "Compiling C code" $ Stage3.C.compile cfg third_fixpoint (dropExtension target)
            else timeIt "Compiling C code" $ MinusMinus.C.compile cfg mmGrin (dropExtension target)


outputGrin file variant grin
    = do let outputFile = replaceExtension file ("grin"++variant)
         writeFile outputFile (show $ ppGrin grin)
         return ()

outputGrin2 file variant grin
    = do let outputFile = replaceExtension file ("grin2"++variant)
         writeFile outputFile (show $ Stage2.ppGrin grin)
         return ()

outputGrin3 file variant grin
    = do let outputFile = replaceExtension file ("grin3"++variant)
         writeFile outputFile (show $ Stage3.ppGrin grin)
         return ()

outputGrinMM file variant grin
    = do let outputFile = replaceExtension file ("grin--"++variant)
         writeFile outputFile (show $ MinusMinus.ppGrin grin)
         return ()

loadAllLibraries :: IO (Map.Map ModuleIdent SimpleModule)
loadAllLibraries
    = do dataDir <- getAppUserDataDirectory "lhc"
         let packageDir = dataDir </> "packages"
         packages <- getDirectoryContents packageDir
         smods <- forM (filter (`notElem` [".",".."]) packages) $ \package ->
                  do modules <- getDirectoryContents (packageDir </> package)
                     forM (filter (`notElem` [".",".."]) modules) $ \mod ->
                       do -- putStrLn $ "Loading: " ++ (packageDir </> package </> mod)
                          smod <- decodeFile (packageDir </> package </> mod)
                          return ((package,mod),smod)
         return $ Map.fromList [ (ident, mod) | (ident,mod) <- concat smods ]


parseCore :: FilePath -> IO SimpleModule
parseCore path
    = do inp <- L.readFile path
         case Core.parseModule path inp of
           Left errs -> error (show errs)
           Right mod -> do --putStrLn $ "parsing done: " ++ path
                           return (coreToSimpleCore mod)

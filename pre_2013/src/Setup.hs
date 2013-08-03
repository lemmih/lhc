module Setup where

import Config                    ( Mode(..), Config(..), defaultConfig, OptConfig(..) )
import System.Console.GetOpt     ( getOpt, usageInfo, OptDescr(..), ArgDescr(..), ArgOrder(..) )
import System.IO                 ( hPutStrLn, stderr )
import System.Environment        ( getProgName, getArgs )
import System.Exit               ( exitWith, ExitCode(..) )
import Control.Monad             ( forM_ )



data Flag
    = VersionFlag
    | NumericVersionFlag
    | SupportedLanguagesFlag
    | InstallLibraryFlag
    | GhcOptionsFlag [String]
    | BuildLibraryFlag
    | NoLinkFlag
    | OutputFileFlag String
    | SourceDirFlag String
    | DebugBuildFlag
    | ProfBuildFlag
    | KeepTmpFilesFlag
    | UseCcFlag String
    | UseGrinMM
    | OptSmallNodeSize Int
    | HelpFlag
    deriving (Eq)

options = [ Option "?h" ["help"] (NoArg HelpFlag) "Prints this info."
          , Option "V" ["version"] (NoArg VersionFlag) "Show version information."
          , Option "" ["numeric-version"] (NoArg NumericVersionFlag) "Raw numeric version output."
          , Option "" ["supported-languages"] (NoArg SupportedLanguagesFlag) "List supported LANGUAGE pragmas"
          , Option "" ["install-library"] (NoArg InstallLibraryFlag) "Don't compile; install modules under a library."

          , Option "" ["ghc-opts"] (ReqArg (GhcOptionsFlag . words) "options") "Command-line options for the GHC front-end."
          , Option "" ["build-library"] (NoArg BuildLibraryFlag) "Used when compiling a library (cabal only)."
          , Option "o" ["output-file"] (ReqArg OutputFileFlag "file") "Output file for binary."
          , Option "" ["src-dir"] (ReqArg SourceDirFlag "directory") "Source code directory"
          , Option "g" ["debug-build"] (NoArg DebugBuildFlag) "build C code without optimizations and with debug mode enabled"
          , Option "" ["prof-build"] (NoArg DebugBuildFlag) "build C code with profiling enabled"
          , Option "" ["grin-backend"] (NoArg UseGrinMM) "target grin--."
          , Option "c" [] (NoArg NoLinkFlag) "stop before linking."
          , Option "" ["keep-tmp-files"] (NoArg KeepTmpFilesFlag) "keep all intermediate object, interface, grin and C files"
          , Option "" ["use-cc"] (ReqArg UseCcFlag "cmd") "specify the C compiler to use"
          , Option "" ["opt-small-node-size"] (ReqArg (OptSmallNodeSize . read) "number") "Maximum size of small nodes. Default is 4 words."
          ]

combine :: Flag -> Config -> Config
combine flag cfg
    = case flag of
        GhcOptionsFlag opts    -> cfg{configGhcOptions = configGhcOptions cfg ++ opts}
        BuildLibraryFlag       -> cfg{configBuildLibrary = True}
        NoLinkFlag             -> cfg{configBuildLibrary = True}
        OutputFileFlag path    -> cfg{configOutputFile = path}
        SourceDirFlag dir      -> cfg{configSrcDir = dir}
        DebugBuildFlag         -> cfg{configDebugBuild = True}
        ProfBuildFlag          -> cfg{configProfBuild = True}
        KeepTmpFilesFlag       -> cfg{configKeepTmpFiles = True}
        UseCcFlag cmd          -> cfg{configUseCc = cmd}
        UseGrinMM              -> cfg{configUseGrinMM = True}
        OptSmallNodeSize size  -> setOpt $ \opt -> opt{optSmallNodeSize = size}
    where setOpt fn = cfg{configOpt = fn (configOpt cfg)}

getMode :: String -> IO Mode
getMode banner = parseArguments banner =<< getArgs

parseArguments :: String -> [String] -> IO Mode
parseArguments banner args
    = case getOpt Permute options args of
        (flags, files, [])
            | HelpFlag `elem` flags
            -> do putStrLn banner
                  printUsage
                  exitWith ExitSuccess
            | VersionFlag `elem` flags
            -> return Version
            | NumericVersionFlag `elem` flags
            -> return NumericVersion
            | SupportedLanguagesFlag `elem` flags
            -> return SupportedLanguages
            | InstallLibraryFlag `elem` flags
            -> return (InstallLibrary files)
            | otherwise
            -> return (Normal (foldr combine defaultConfig flags) files)
        (flags, _files, errs)
          -> do forM_ errs $ hPutStrLn stderr
                printUsage
                exitWith (ExitFailure 1)
    where printUsage = do prog <- getProgName
                          hPutStrLn stderr (usageInfo prog options)
                

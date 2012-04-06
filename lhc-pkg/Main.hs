module Main where

import System.Cmd
import System.Environment
import System.FilePath
import System.Directory
import System.Info
import System.Exit
import Data.Version as Version
import Paths_lhc

main :: IO ()
main = do pkgConf <- getPkgConf
          args <- getArgs
          case args of
            ["--version"] -> putStrLn $ "LHC package manager version " ++ Version.showVersion version
            ["--help"]    -> showHelp
            ["-h"]        -> showHelp
            _other        -> exitWith =<< system (unwords (["ghc-pkg"]++ args ++
                                                           ["--package-conf=" ++ pkgConf
                                                           ,"--global-conf=" ++ pkgConf
                                                           ,"--no-user-package-conf"]))

showHelp = putStrLn "lhc-pkg works just like ghc-pkg. please run `ghc-pkg --help` for more info"

getPkgConf
    = do appdir <- getAppUserDataDirectory "lhc"
         let targetARCH = arch
             targetOS   = os
         let subdir = targetARCH ++ '-':targetOS ++ '-':Version.showVersion version
         return (appdir </> subdir </> "package.conf.d")

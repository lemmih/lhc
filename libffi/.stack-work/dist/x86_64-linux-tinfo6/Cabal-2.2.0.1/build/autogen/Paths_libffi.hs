{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_libffi (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/lemmih/Coding/Haskell/lhc/.stack-work/install/x86_64-linux-tinfo6/lts-12.26/8.4.4/bin"
libdir     = "/home/lemmih/Coding/Haskell/lhc/.stack-work/install/x86_64-linux-tinfo6/lts-12.26/8.4.4/lib/x86_64-linux-ghc-8.4.4/libffi-0.2-LFHFrVpfC6KAgELjP9tfdc"
dynlibdir  = "/home/lemmih/Coding/Haskell/lhc/.stack-work/install/x86_64-linux-tinfo6/lts-12.26/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/lemmih/Coding/Haskell/lhc/.stack-work/install/x86_64-linux-tinfo6/lts-12.26/8.4.4/share/x86_64-linux-ghc-8.4.4/libffi-0.2"
libexecdir = "/home/lemmih/Coding/Haskell/lhc/.stack-work/install/x86_64-linux-tinfo6/lts-12.26/8.4.4/libexec/x86_64-linux-ghc-8.4.4/libffi-0.2"
sysconfdir = "/home/lemmih/Coding/Haskell/lhc/.stack-work/install/x86_64-linux-tinfo6/lts-12.26/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "libffi_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "libffi_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "libffi_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "libffi_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "libffi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "libffi_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

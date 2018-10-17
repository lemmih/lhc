module RTS (linkRTS) where

import           Control.Monad    (unless)
import           Paths_lhc        (getDataFileName)
import           System.Directory (findExecutable, getTemporaryDirectory)
import           System.Exit      (ExitCode (..))
import           System.FilePath  ((</>))
import           System.Process   (readProcessWithExitCode)

-- Compile the RTS (written in c) with clang and link the files directly
-- into the target code.
linkRTS :: FilePath -> IO ()
linkRTS target = do
  rtsDir <- getDataFileName "rts"
  semispace <- getDataFileName "rts/gc/semispace.c"
  api <- getDataFileName "rts/api.c"
  stats <- getDataFileName "rts/stats.c"
  clang <- findLLVMExecutable "clang"
  link <- findLLVMExecutable "llvm-link"
  tmpDir <- getTemporaryDirectory
  let semi_dst = tmpDir </> "semispace.ll"
      api_dst = tmpDir </> "api.ll"
      stats_dst = tmpDir </> "stats.ll"
  assertProcess clang ["-O2", "-I"++rtsDir, semispace, "-emit-llvm", "-Werror", "-S", "-o",semi_dst]
  assertProcess clang ["-O2", "-I"++rtsDir, api, "-emit-llvm", "-Werror", "-S", "-o",api_dst]
  assertProcess clang ["-O2", "-I"++rtsDir, stats, "-emit-llvm", "-Werror", "-S", "-o",stats_dst]
  assertProcess link [target, semi_dst, api_dst, stats_dst, "-S", "-o", target]




----------------------------------------------------------------------
-- Helpers


findLLVMExecutable :: FilePath -> IO FilePath
findLLVMExecutable exec = do
  mbRet <- findExecutable exec
  case mbRet of
    Just path -> pure path
    Nothing -> do
      mbV6 <- findExecutable (exec++"-6.0")
      case mbV6 of
        Just path -> pure path
        Nothing   -> error $ "Required program could not be found: " ++ exec


assertProcess :: String -> [String] -> IO ()
assertProcess prog args = do
  (code, _stdout, stderr) <- readProcessWithExitCode prog args ""
  unless (null stderr) $
    error $ prog ++ " failed with message:\n" ++ stderr
  case code of
    ExitSuccess -> return ()
    ExitFailure val ->
      error $ prog ++ " failed with error code: " ++ show val

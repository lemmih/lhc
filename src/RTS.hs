module RTS (linkRTS) where

import           Control.Monad    (unless)
import           Paths_lhc        (getDataFileName)
import           System.Directory (findExecutable, getTemporaryDirectory)
import           System.Exit      (ExitCode (..))
import           System.FilePath  ((</>))
import           System.Process   (readProcessWithExitCode)

linkRTS :: FilePath -> IO ()
linkRTS target = do
  rtsDir <- getDataFileName "rts"
  semispace <- getDataFileName "rts/gc/semispace.c"
  clang <- findLLVMExecutable "clang"
  link <- findLLVMExecutable "llvm-link"
  tmpDir <- getTemporaryDirectory
  let dst = tmpDir </> "semispace.ll"
  assertProcess clang ["-I"++rtsDir, semispace, "-emit-llvm", "-S", "-o",dst]
  assertProcess link [target, dst, "-S", "-o", target]




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

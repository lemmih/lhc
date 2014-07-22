module Main where

import Data.Tagged
import Text.PrettyPrint.ANSI.Leijen
import System.FilePath

import Language.Haskell.Exts.Annotated

import Language.Haskell.TypeCheck.Monad
import Language.Haskell.TypeCheck.Infer
--import Language.Haskell.TypeCheck.Types
import Language.Haskell.Scope

import qualified Compiler.HaskellToCore          as Haskell
import qualified Compiler.CoreToBedrock as Core
import           Data.Bedrock.PrettyPrint
import qualified Data.Bedrock.Compile as Bedrock


import qualified Distribution.HaskellSuite.Compiler as Compiler
import Distribution.HaskellSuite.Packages

import Paths_lhc

testInfer :: IO ()
testInfer = do
    ParseOk m <- parseFile "src/Test.hs"
    let (errs, m') = resolve m
    mapM_ print errs
    _env <- runTI (tiModule m')
    return ()

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
main = Compiler.main lhcCompiler

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
        compile
        ["hi", "core"]

compile :: Compiler.CompileFn
compile buildDir mbLang exts cppOpts pkgName pkgdbs deps [file] = do
    putStrLn "Parsing file..."
    ParseOk m <- parseFile file
    putStrLn "Origin analysis..."
    let (errs, m') = resolve m
    mapM_ print errs
    putStrLn "Typechecking..."
    env <- runTI (tiModule m')
    putStrLn "Converting to core..."
    let core = Haskell.convert env m'
    print (pretty core)
    let bedrock = Core.convert core
    print (ppModule bedrock)
    let bedrockFile = replaceExtension file "bedrock"
    writeFile bedrockFile (show $ ppModule bedrock)
    Bedrock.compileModule bedrock file
    return ()


module Main where

import Language.Haskell.Exts.Annotated

import Language.Haskell.TypeCheck.Monad
import Language.Haskell.TypeCheck.Infer
--import Language.Haskell.TypeCheck.Types
import Language.Haskell.Scope

import Compiler.CoreToBedrock()

testInfer :: IO ()
testInfer = do
    ParseOk m <- parseFile "src/Test.hs"
    let (errs, m') = resolve m
    mapM_ print errs
    _env <- runTI (tiModule m')
    return ()

main :: IO ()
main = return ()

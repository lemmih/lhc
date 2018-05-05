module Main where

import LHC.Prim
import LHC.Prelude

main :: IO ()
main = putStrLn (digits (last longList))

longList :: [Int]
longList = replicate 100 0

entrypoint :: ()
entrypoint = unsafePerformIO main

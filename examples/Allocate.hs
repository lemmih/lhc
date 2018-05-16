module Main where

import LHC.Prim
import LHC.Prelude

main :: IO ()
main = putStrLn (showInt (last longList))

longList :: [Int]
longList = replicate 1000000 0

entrypoint :: ()
entrypoint = unsafePerformIO main

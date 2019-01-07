module Main where

import LHC.Prim
import LHC.Prelude

main :: IO ()
main = putStrLn (showInt (last (reverse longList)))

longList :: [Int]
longList = replicate 100000000 0

entrypoint :: ()
entrypoint = unsafePerformIO main

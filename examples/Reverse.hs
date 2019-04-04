module Main where

import LHC.Prim
import LHC.Prelude

main :: IO ()
main = putStrLn (showInt (last (reverse longList)))

longList :: [Int]
longList = replicate 2000 0

entrypoint :: ()
entrypoint = unsafePerformIO main

module Main where

import LHC.Prim
import LHC.Prelude

main :: IO ()
main = do
    putStr "head ones = "
    putStrLn (showInt (head ones))
  where
    ones :: [Int]
    ones = 1 : ones

entrypoint :: ()
entrypoint = unsafePerformIO main

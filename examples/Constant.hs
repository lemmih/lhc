module Main where

import LHC.Prim
import LHC.Prelude

n :: Int
n = 10

main :: IO ()
main = do
  putStr "n = "
  putStrLn (showInt n)

entrypoint :: ()
entrypoint = unsafePerformIO main

module Main where

import LHC.Prim
import LHC.Prelude

main :: IO ()
main = do
    putStr "n = "
    putStrLn (showInt n)
  where
    n :: Int
    n = 10

entrypoint :: ()
entrypoint = unsafePerformIO main

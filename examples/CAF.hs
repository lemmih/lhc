module Main where

import LHC.Prim
import LHC.Prelude

hundred :: [Char]
hundred = showInt 100

main :: IO ()
main = do
  putStr "hundred = "
  putStrLn hundred

entrypoint :: ()
entrypoint = unsafePerformIO main

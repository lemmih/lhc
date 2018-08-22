module Main (main) where

import LHC.Prim
import LHC.Prelude

main :: IO ()
main = do
  printShift 1 1
  printShift 1 2
  printShift 1 3
  printShift 1 4

printShift :: Int -> Int -> IO ()
printShift a b = do
  putStr (showInt a)
  putStr " << "
  putStr (showInt b)
  putStr " = "
  putStrLn (showInt (shiftL a b))

entrypoint :: ()
entrypoint = unsafePerformIO main

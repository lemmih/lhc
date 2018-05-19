module Main where

import LHC.Prim
import LHC.Prelude

{-# NOINLINE hundred #-}
hundred :: [Char]
hundred = unsafePerformIO (do
  putStr "(This should only be shown once) "
  return (showInt 100))

main :: IO ()
main = do
  putStr "hundred = "
  putStrLn hundred
  putStr "hundred = "
  putStrLn hundred

entrypoint :: ()
entrypoint = unsafePerformIO main

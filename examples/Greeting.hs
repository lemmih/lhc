{-# LANGUAGE MagicHash #-}
module Main (main) where

import LHC.Prim

main :: IO ()
main = do
  putStrLn "What is your name?"
  name <- getLine
  putStr "Hi "
  putStr name
  putStrLn "."

entrypoint :: ()
entrypoint = unsafePerformIO main

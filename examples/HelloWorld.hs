{-# LANGUAGE MagicHash #-}
module Main (main) where

import LHC.Prim

main :: IO ()
main = putStrLn "Hello world!"

entrypoint :: ()
entrypoint = unsafePerformIO main

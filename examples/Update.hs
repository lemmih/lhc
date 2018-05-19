module Main where

import LHC.Prim
import LHC.Prelude

main :: IO ()
main = dup (unsafePerformIO (do putStrLn "This message should only be shown once"
                                return 10))

dup :: Int -> IO ()
dup a = force a a

force :: Int -> Int -> IO ()
force a b = do
  putStrLn "This message should come first"
  putStrLn (showInt (a+b))

entrypoint :: ()
entrypoint = unsafePerformIO main

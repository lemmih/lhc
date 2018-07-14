module Main where

import LHC.Prim

main :: IO ()
main = do
  args <- getArgs
  mapM_ putStrLn args

entrypoint :: ()
entrypoint = unsafePerformIO main

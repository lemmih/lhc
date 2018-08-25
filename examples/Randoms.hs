module Main where

import LHC.Prim
import LHC.Prelude

randoms :: Int -> [Int]
randoms n = take n (iterate (\seed -> srem (77*seed+1) 1024) 1967)

main :: IO ()
main = mapM_ print (randoms 10)

entrypoint :: ()
entrypoint = unsafePerformIO main

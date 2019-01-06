module Main where

import LHC.Prim
import LHC.Prelude

run :: Int -> [[Int]]
run n = power n n []

power :: Int -> Int -> [Int] -> [[Int]]
power 0 0 acc = acc : []
power n 0 acc = acc : power (n-1) (n `sdiv` 2) acc
power n i acc = power n (i-1) (i:acc)

main :: IO ()
main = do
  putStrLn "Allocating a bunch of data"
  putStrLn (showInt (last (last (reverse (run 20000)))))

entrypoint :: ()
entrypoint = unsafePerformIO main

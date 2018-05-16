module Main where

import LHC.Prim
import LHC.Prelude

allocate :: Int -> Int -> [Int] -> [[Int]]
allocate 0 0 acc = acc : []
allocate n 0 acc = acc : allocate (n-1) 1024 []
allocate n i acc = allocate n (i-1) (i:acc)

main :: IO ()
main = do
  putStrLn "Allocating a bunch of data"
  putStrLn (showInt (last (last (reverse (allocate 1024 1024 [])))))

entrypoint :: ()
entrypoint = unsafePerformIO main

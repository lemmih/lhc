module Main ( main ) where

main :: IO ()
main = do
  print $ [ x | xs <- [ [(1,2),(3,4)], [(5,4),(3,2)] ], (3,x) <- xs ] == [ 4, 2 ]

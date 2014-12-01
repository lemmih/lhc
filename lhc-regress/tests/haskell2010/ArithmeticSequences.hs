module Main ( main ) where

main :: IO ()
main = do
  print $ take 100 [ 1 .. ] == take 100 (enumFrom 1)
  print $ take 100 [ 1 , 4 .. ] == take 100 (enumFromThen 1 4)
  print $ [ 1 .. 4 ] == enumFromTo 1 4
  print $ [ 1 , 4 .. 100 ] == enumFromThenTo 1 4 100

module Main where

import LHC.Prim
import LHC.Prelude

data Tree = Leaf | Branch Tree Tree Tree Tree

make :: Int -> Tree
make 0 = Leaf
make 1 = Branch (make 0) (make 0) (make 0) (make 0)
make 2 = Branch (make 1) (make 1) (make 1) (make 1)
make 3 = Branch (make 2) (make 2) (make 2) (make 2)
make 4 = Branch (make 3) (make 3) (make 3) (make 3)
make 5 = Branch (make 4) (make 4) (make 4) (make 4)
make 6 = Branch (make 5) (make 5) (make 5) (make 5)
make 7 = Branch (make 6) (make 6) (make 6) (make 6)
make 8 = Branch (make 7) (make 7) (make 7) (make 7)
make 9 = Branch (make 8) (make 8) (make 8) (make 8)
make 10 = Branch (make 9) (make 9) (make 9) (make 9)
make 11 = Branch (make 10) (make 10) (make 10) (make 10)
make 12 = Branch (make 11) (make 11) (make 11) (make 11)
make n = Branch (make (n-1)) (make (n-1)) (make (n-1)) (make (n-1))

check :: Tree -> [Tree] -> Int -> Int
check Leaf lst n =
  case lst of
    [] -> 1+n
    (x:xs) -> check x xs (1+n)
check (Branch a b c d) lst n =
  check a (b:c:d:lst) (1+n)

main :: IO ()
main = dup (make 12)

dup a = act a a

act :: Tree -> Tree -> IO ()
act a b = do
  putStrLn (showInt (check a [] 0))
  putStrLn (showInt (check b [] 0))


entrypoint :: ()
entrypoint = unsafePerformIO main

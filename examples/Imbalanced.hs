module Main where

import LHC.Prim
import LHC.Prelude

data Tree = Leaf | Branch Tree Tree

make :: Int -> Tree
make 0 = Leaf
make 1 = Branch Leaf Leaf
make n = Branch (make (n-2)) (make (n-1))

check :: Tree -> Int
check Leaf = 0
check (Branch l r) = 1 + check r + check l

main :: IO ()
main = dup (make 34)

dup a = act a a

act :: Tree -> Tree -> IO ()
act a b = do
  putStrLn (showInt (check a))
  putStrLn (showInt (check b))


entrypoint :: ()
entrypoint = unsafePerformIO main

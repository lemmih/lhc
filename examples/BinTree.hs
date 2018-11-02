module Main where

import LHC.Prim
import LHC.Prelude

data Tree = Leaf | Branch Tree Tree

make :: Int -> Tree
make 0 = Leaf
make n = Branch (make (n-1)) (make (n-1))

check :: Tree -> Int
check Leaf = 1
check (Branch l r) = 3 + check l + check r

main :: IO ()
main = dup (make 23)

dup a = act a a

act :: Tree -> Tree -> IO ()
act a b = do
  putStrLn (showInt (check a))
  putStrLn (showInt (check b))


entrypoint :: ()
entrypoint = unsafePerformIO main

module Main where

import LHC.Prim
import LHC.Prelude

data Tree = Leaf | Branch Tree Tree Tree Tree

make :: Int -> Tree
make 0 = Leaf
make n = Branch (make (n-1)) (make (n-1)) (make (n-1)) (make (n-1))

check :: Tree -> Int
check Leaf = 1
check (Branch a b c d) = 1 + check a + check b + check c + check d

main :: IO ()
main = dup (make 12)

dup a = act a a

act :: Tree -> Tree -> IO ()
act a b = do
  putStrLn (showInt (check a))
  putStrLn (showInt (check b))


entrypoint :: ()
entrypoint = unsafePerformIO main

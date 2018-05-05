{-# LANGUAGE MagicHash #-}
module LHC.Prelude
  ( reverse
  , append
  , showInt
  , digits
  , last
  , replicate ) where

import LHC.Prim

reverse :: [a] -> [a]
reverse lst = reverse_go [] lst

reverse_go :: [a] -> [a] -> [a]
reverse_go acc lst =
  case lst of
    []   -> acc
    x:xs -> reverse_go (x : acc) xs

append :: [a] -> [a] -> [a]
append [] b     = b
append (x:xs) b = x : append xs b

intToDigit :: Int -> Char
intToDigit 0 = '0'
intToDigit 1 = '1'
intToDigit 2 = '2'
intToDigit 3 = '3'
intToDigit 4 = '4'
intToDigit 5 = '5'
intToDigit 6 = '6'
intToDigit 7 = '7'
intToDigit 8 = '8'
intToDigit 9 = '9'

showInt :: Int -> [Char]
showInt 0 = '0' : []
showInt x = reverse (digits x)

digits :: Int -> [Char]
digits 0 = "0"
digits n = intToDigit (srem n 10) : digits_go (sdiv n 10)

digits_go 0 = []
digits_go n = intToDigit (srem n 10) : digits_go (sdiv n 10)

-- digits (I# i) =
--   case i32toi64 i of
--     0# -> []
--     _  -> intToDigit (srem (I# i) 10) : digits (sdiv (I# i) 10)

last :: [a] -> a
last [] = last []
last (x:xs) = last_go x xs

last_go x [] = x
last_go x (y:ys) = last_go y ys

replicate :: Int -> a -> [a]
replicate 0 elt = []
replicate n elt = elt : replicate (n-1) elt

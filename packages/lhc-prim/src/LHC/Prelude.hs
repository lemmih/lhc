{-# LANGUAGE MagicHash #-}
module LHC.Prelude
  ( reverse
  , append ) where

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
intToDigit (I# i) =
  case i32toi64 i of
    0# -> '0'
    1# -> '1'
    2# -> '2'
    3# -> '3'
    4# -> '4'
    5# -> '5'
    6# -> '6'
    7# -> '7'
    8# -> '8'
    9# -> '9'

showInt :: Int -> [Char]
showInt 0 = '0' : []
showInt x = reverse (digits x)

ten :: Int
ten = I# (i64toi32 10#)

digits :: Int -> [Char]
digits (I# i) =
  case i32toi64 i of
    0# -> []
    _  -> intToDigit (srem (I# i) ten) : digits (sdiv (I# i) ten)


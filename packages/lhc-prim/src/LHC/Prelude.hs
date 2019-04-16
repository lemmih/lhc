{-# LANGUAGE MagicHash #-}
module LHC.Prelude
  ( reverse
  , append
  , showInt
  , digits
  , last
  , head
  , replicate
  , take
  , iterate
  , print
  ) where

import LHC.Prim

reverse :: [a] -> [a]
reverse lst = reverse_go [] lst
  where
    reverse_go :: [a] -> [a] -> [a]
    reverse_go acc lst =
      case lst of
        []   -> acc
        x:xs -> reverse_go (x : acc) xs

append :: [a] -> [a] -> [a]
append [] b     = b
append (x:xs) b = x : append xs b

-- digitToInt :: Char -> Int
-- digitToInt '0' = 0
-- digitToInt '1' = 1
-- digitToInt '2' = 2
-- digitToInt '3' = 3
-- digitToInt '4' = 4
-- digitToInt '5' = 5
-- digitToInt '6' = 6
-- digitToInt '7' = 7
-- digitToInt '8' = 8
-- digitToInt '9' = 9
--
-- readInt :: [Char] -> Int
-- readInt str = readInt_go (reverse str)
--
-- readInt_go [] = 0
-- readInt_go (x:xs) = digitToInt x + (readInt_go xs * 10)

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

head :: [a] -> a
head [] = head []
head (x:xs) = x

last :: [a] -> a
last [] = last []
last (x:xs) = last_go x xs

last_go x [] = x
last_go x (y:ys) = last_go y ys

-- replicate :: Int -> a -> [a]
-- replicate 0 elt = []
-- replicate n elt = elt : replicate (n-1) elt

replicate :: Int -> a -> [a]
replicate (I# n) elt = replicate_go n elt

replicate_go '\0'# elt = []
replicate_go n elt = elt : replicate_go (n -# '\1'#) elt

take :: Int -> [a] -> [a]
take 0 _ = []
take n (x:xs) = x : take (n-1) xs

iterate :: (a -> a) -> a -> [a]
iterate fn a = a : iterate fn (fn a)

print :: Int -> IO ()
print n = putStrLn (showInt n)

module Fold where

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr fn acc [] = acc
foldr fn acc (x:xs) = fn x (foldr fn acc xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl fn acc [] = acc
foldl fn acc (x:xs) = foldl fn (fn acc x) xs

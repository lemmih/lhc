module Map where

map :: (a -> b) -> [a] -> [b]
map f lst =
  case lst of
    []     -> []
    (x:xs) -> f x : map f xs

module Main ( main ) where

data List a = Cons a (List a) | Nil

main :: IO ()
main =
  case Nil of
    Cons x (Cons y (Cons z Nil)) -> return ()
    Cons x xs -> return ()
    Nil -> return ()

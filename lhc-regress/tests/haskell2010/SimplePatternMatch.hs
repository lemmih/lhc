module Main ( main ) where

data List a = Cons a (List a) | Nil

main :: IO ()
main =
  case Nil of
    Cons x xs -> return ()
    Nil       -> return ()


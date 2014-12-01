module Main ( main ) where

main :: IO ()
main =
  case val of
    ((),((), x)) -> return x

val :: ((), ((), ()))
val = ((), (case val of (x,_) -> x, ()))

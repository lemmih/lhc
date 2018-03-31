{-# LANGUAGE MagicHash #-}
module Main (main) where

import LHC.Prim

-- main :: IO ()
-- main = do
--   putStrLn (unpackString# "What is your name?"#)
--   name <- getLine
--   putStr (unpackString# "Hi "#)
--   putStr name
--   putStrLn (unpackString# "."#)

main =
  putStrLn (unpackString# "What is your name?"#) `thenIO`
  (getLine `bindIO` \name ->
  (putStr (unpackString# "Hi "#) `thenIO`
  (putStr name `thenIO`
  (putStrLn (unpackString# "."#)))))

entrypoint :: ()
entrypoint = unsafePerformIO main

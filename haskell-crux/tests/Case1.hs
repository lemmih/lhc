{-# LANGUAGE UnboxedTuples, MagicHash #-}
module Case1 where

x = case (# 10 , 20 #) of
      (# a, b #) -> a

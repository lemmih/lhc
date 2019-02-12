module Basic1 where

x = y
  where y = x

fn1 0 = y
  where
    y = 10

fn2 n = y where y = fn2 n

fn3 n = even
  where
    even = 0 : odd
    odd = 1 : even

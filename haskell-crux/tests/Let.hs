module Basic1 where

x = let y = x in y

fn1 0 = let y = 10 in y

fn2 n = let y = fn2 n in y

fn3 n = let even = 0 : odd
            odd = 1 : even
        in even

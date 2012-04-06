module Simple.IO where

import GHC.Base
import GHC.Types

foreign import ccall unsafe "putchar" c_putchar :: Int -> IO Int

putString :: String -> IO ()
putString []     = return ()
putString (x:xs) = putChar x >> putString xs

putChar :: Char -> IO ()
putChar char
  = do c_putchar (ord char)
       return ()


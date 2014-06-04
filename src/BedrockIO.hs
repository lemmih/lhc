{-# LANGUAGE ForeignFunctionInterface, NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
module Main where


data RealWorld
data IO a = IO (RealWorld -> IOUnit a)
unIO :: IO a -> (RealWorld -> IOUnit a)
unIO io =
  case io of
    IO action -> action

data Ptr a
data I8
data I32
data I64
data Addr a

data Int = Int I64
data Int32 = Int32 I32
data Ptr a = Ptr (Addr I8)

data IOUnit a = IOUnit a RealWorld


--foreign import ccall returnIO :: a -> IO a
--foreign import ccall bindIO :: IO a -> (a -> IO b) -> IO b
--foreign import ccall unsafePerformIO :: IO a -> a

foreign import ccall unsafe realWorld :: RealWorld
unsafePerformIO io =
  case unIO io realWorld of
    IOUnit a s' -> a

returnIO :: a -> IO a
returnIO a = IO (\s -> IOUnit a s)

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO f g = IO (\s ->
  case unIO f s of
    IOUnit a s' -> unIO (g a) s')

thenIO :: IO a -> IO b -> IO b
thenIO a b = bindIO a (\c -> b)

--withContinuation :: ((a -> Cont) -> IO ()) -> IO a
--continue :: Cont -> IO a

foreign import ccall unsafe puts :: Addr I8 -> IO I32


main :: IO ()
main =
    puts "hello"# `thenIO`
    puts "world"# `thenIO`
    puts "Haskell"# `thenIO`
    puts "Rocks!"#

entryPoint :: ()
entryPoint = unsafePerformIO main

{-
void main()
  str1 = literal "Hello"
  putStrLn(str1)
  str2 = literal "World"
  putStrLn(str2)

-}




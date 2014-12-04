{-# LANGUAGE MagicHash, ForeignFunctionInterface #-}
{-# LANGUAGE UnboxedTuples #-}
module LHC.Prim
    ( IO
    , Unit(Unit)
    , I8
    , I64
    , Addr
    , puts
    , bindIO
    , thenIO
    , return
    , unsafePerformIO
    , List(Nil,Cons)
    , sequence_
    , unpackString#
    , putStr
    , Char
    ) where

-- XXX: These have kind # but it is not checked anywhere.
--      Use them incorrectly and the world will explode.
data I8
data I32
data I64
data Addr a

data Int32 = Int32 I32

data Char = C# I64

data RealWorld = RealWorld

-- data IOUnit a = IOUnit a RealWorld
--newtype IO a = IO { unIO :: RealWorld -> IOUnit a }
newtype IO a = IO (RealWorld -> (# RealWorld, a #))

data Unit = Unit
data List a = Nil | Cons a (List a)

unIO :: IO a -> RealWorld -> (# RealWorld, a #)
unIO action =
  case action of
    IO unit -> unit

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO f g = IO (\s ->
  case unIO f s of
    (# s', a #) -> unIO (g a) s)

thenIO :: IO a -> IO b -> IO b
thenIO a b = bindIO a (\c -> b)

return :: a -> IO a
return a = IO (\s -> (# s, a #))

unsafePerformIO :: IO a -> a
unsafePerformIO action =
    case unIO action RealWorld of
        (# st, val #) -> val

foreign import ccall unsafe "puts" puts :: Addr I8 -> IO Int32
foreign import ccall unsafe "putchar" putchar :: I32 -> IO Int32

-- sequence_ :: List (IO Unit) -> IO Unit
-- sequence_ lst =
--   case lst of
--     Nil -> return Unit
--     Cons head tail -> thenIO head (sequence_ tail)

foreign import ccall unsafe indexI8# :: Addr I8 -> I8
foreign import ccall addrAdd# :: Addr I8 -> I64 -> Addr I8
foreign import ccall "cast" i64toi32 :: I64 -> I32
foreign import ccall "cast" i8toi64 :: I8 -> I64

unpackString# :: Addr I8 -> List Char
unpackString# ptr =
  case i8toi64 (indexI8# ptr) of
    0#   -> Nil
    char -> Cons (C# char) (unpackString# (addrAdd# ptr 1#))


putStr :: List Char -> IO Unit
putStr lst =
  case lst of
    Nil -> return Unit
    Cons head tail ->
      case head of
        C# char ->
          putchar (i64toi32 char) `thenIO` putStr tail

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
    ) where

data I8
data I32
data I64
data Addr a

data Int32 = Int32 I32

data RealWorld = RealWorld

-- data IOUnit a = IOUnit a RealWorld
--newtype IO a = IO { unIO :: RealWorld -> IOUnit a }
newtype IO a = IO (RealWorld -> (# RealWorld, a #))

data Unit = Unit

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


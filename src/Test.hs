{-# LANGUAGE MagicHash, KindSignatures #-}
module Test where

data Bool = True | False
--data List a = Nil | Cons a (List a)
data Maybe a = Nothing | Just a

data Addr a
data I8
data I32

data RealWorld
newtype IO a = IO { unIO :: RealWorld -> IOUnit a }
data I8
data I32
data I64
data Addr a

data Int = I64 I64
data Int32 = I32 I32
data Ptr a = Ptr (Addr I8)

data IOUnit a = IOUnit a RealWorld

--returnIO :: a -> IO a
--returnIO a s = IOUnit a s

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO f g = IO (\s ->
  case unIO f s of
    IOUnit a s' -> unIO (g a) s')

thenIO a b = bindIO a (\c -> b)

primTest a b =
    case a of
        I64 a# ->
            case b of
                I64 b# -> I64 (a# +# b#)

foreign import ccall (+#) :: I64 -> I64 -> I64

foreign import ccall puts :: Addr I8 -> IO I32

--class Not a where
--    not :: a -> Bool


--instance Not Bool where
--    not True = False
--    not False = True

--notAlias x = not x

--instance Not a => Not (List a) where
--    not Nil = Nil
--    not (Cons v vs) = Cons (not v) (not vs)

--isJust maybe = case maybe of Nothing -> False; _ -> True

--doNot x = not x
--not True = False
--not False = True

--fromJust def Nothing = def
--fromJust _ (Just val) = val


{-
haskell-scope
haskell-typecheck

Test.True :: Test.Bool
Test.False :: Test.Bool
Test.Nothing :: forall a. Test.Maybe a
Test.Just :: forall a. a -> Test.Maybe a

-}

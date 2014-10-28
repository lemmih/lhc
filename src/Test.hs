{-# LANGUAGE MagicHash, KindSignatures, RankNTypes #-}
module Test where

import Data.Typeable

{-
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


-}


class Depth a where
    depth :: a -> Int

instance Depth () where
    depth ~() = 0

instance Depth a => Depth (Maybe a) where
    depth ~(Just a) = 1 + depth a

--depthMethod :: Type a -> a -> Int
--depthMethod ty v =
--    case ty of
--        Unit -> depthUnit v
--        Maybe subTy -> depthMaybe subTy v

--depthUnit :: () -> Int
--depthUnit _ = 0

--depthMaybe :: Type a -> Maybe a -> Int
--depthMaybe ty ~(Just a) = 1 + depthMethod ty a


mkDepth :: Depth a => Int -> a -> Int
mkDepth 0 v = depth v
mkDepth n v = mkDepth (n-1) (Just v)

--mkDepth :: /\a -> Int -> a -> Int
--mkDepth a 0 v = depth a v
--mkDepth a n v = mkDepth (Maybe a) (n-1) (Just v)

fn :: (forall a. a -> a) -> ((), Bool)
fn i = (i (), i False)

fn :: (/\a. a -> a) -> ((), Bool)
fn = \(i::/\a. a -> a) -> ( i () (), i Bool False)


k :: forall a b. a -> b -> b
f1 :: (Int -> Int -> Int) -> Int
f2 :: (forall x. x -> x -> x) -> Int

expr :: Int
expr = f1 (k Int Int)

expr2 :: Int
expr2 = f2 (k Int Int)

{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, MagicHash, UnliftedFFITypes #-}
module GHC.Integer.Ltm where

import GHC.Types
import GHC.Prim

type Mp_int = Addr#

foreign import ccall unsafe "lhc_mp_cmp" mp_cmp :: Mp_int -> Mp_int -> Int#
foreign import ccall unsafe "lhc_mp_get_int" mp_get_int :: Mp_int -> Int#

foreign import ccall unsafe "lhc_mp_from_int" mp_from_int :: Int# -> Mp_int
foreign import ccall unsafe "lhc_mp_mul" mp_mul :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_add" mp_add :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_sub" mp_sub :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_or" mp_or :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_and" mp_and :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_xor" mp_xor :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_gcd" mp_gcd :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_lcm" mp_lcm :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_quot" mp_quot :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_rem" mp_rem :: Mp_int -> Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_abs" mp_abs :: Mp_int -> Mp_int
foreign import ccall unsafe "lhc_mp_negate" mp_negate :: Mp_int -> Mp_int


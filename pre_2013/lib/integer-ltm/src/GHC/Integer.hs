module GHC.Integer
    ( Integer
    , toInt#
    , eqInteger
    , neqInteger
    , ltInteger
    , leInteger
    , gtInteger
    , geInteger
    , compareInteger
    , quotRemInteger
    , plusInteger
    , minusInteger
    , timesInteger
    , negateInteger
    , absInteger
    , signumInteger
    , smallInteger
    , quotInteger
    , remInteger
    , divModInteger
    , lcmInteger
    , gcdInteger
    , andInteger
    , orInteger
    , xorInteger
    , complementInteger

#if WORD_SIZE == 4
    , integerToWord64
    , integerToInt64
    , word64ToInteger
    , int64ToInteger
#endif

    , wordToInteger
    , integerToWord
    , floatFromInteger
    , doubleFromInteger
    ) where

import GHC.Types
import GHC.Prim
import GHC.IntWord64

import GHC.Integer.Ltm
import GHC.Integer.Type

toInt# :: Integer -> Int#
toInt# (Integer a) = mp_get_int a

eqInteger :: Integer -> Integer -> Bool
eqInteger (Integer a) (Integer b) = mp_cmp a b ==# 0#

neqInteger :: Integer -> Integer -> Bool
neqInteger (Integer a) (Integer b) = mp_cmp a b /=# 0#

ltInteger :: Integer -> Integer -> Bool
ltInteger (Integer a) (Integer b) = mp_cmp a b ==# (-1#)

leInteger :: Integer -> Integer -> Bool
leInteger (Integer a) (Integer b) = mp_cmp a b /=# 1#

gtInteger :: Integer -> Integer -> Bool
gtInteger (Integer a) (Integer b) = mp_cmp a b ==# 1#

geInteger :: Integer -> Integer -> Bool
geInteger (Integer a) (Integer b) = mp_cmp a b /=# (-1#)

compareInteger :: Integer -> Integer -> Ordering
compareInteger (Integer a) (Integer b)
    = case mp_cmp a b of
        (-1#) -> LT
        0#    -> EQ
        1#    -> GT

quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger a b = (# quotInteger a b, remInteger a b #)

#define BIN_OP(fn) \(Integer a) (Integer b) -> Integer (fn a b)
#define UN_OP(fn) \(Integer a) -> Integer (fn a)

plusInteger :: Integer -> Integer -> Integer
plusInteger = BIN_OP(mp_add)

minusInteger :: Integer -> Integer -> Integer
minusInteger = BIN_OP(mp_sub)

timesInteger :: Integer -> Integer -> Integer
timesInteger = BIN_OP(mp_mul)

negateInteger :: Integer -> Integer
negateInteger = UN_OP(mp_negate)

absInteger :: Integer -> Integer
absInteger = UN_OP(mp_abs)

signumInteger :: Integer -> Integer
signumInteger i = case compareInteger i (smallInteger 0#) of
                    LT -> smallInteger (-1#)
                    EQ -> smallInteger (0#)
                    GT -> smallInteger (1#)

smallInteger :: Int# -> Integer
smallInteger val = Integer (mp_from_int val)

quotInteger :: Integer -> Integer -> Integer
quotInteger = BIN_OP(mp_quot)

remInteger :: Integer -> Integer -> Integer
remInteger = BIN_OP(mp_rem)

divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger a b = (# divInteger a b, modInteger a b #)

lcmInteger :: Integer -> Integer -> Integer
lcmInteger = BIN_OP(mp_lcm)

gcdInteger :: Integer -> Integer -> Integer
gcdInteger = BIN_OP(mp_gcd)

andInteger :: Integer -> Integer -> Integer
andInteger = BIN_OP(mp_and)

orInteger :: Integer -> Integer -> Integer
orInteger = BIN_OP(mp_or)

xorInteger :: Integer -> Integer -> Integer
xorInteger = BIN_OP(mp_xor)

complementInteger :: Integer -> Integer
complementInteger i = negateInteger i `minusInteger` smallInteger 1#


#if WORD_SIZE == 4
integerToWord64 :: Integer -> Word64#
integerToWord64 = integerToWord64

integerToInt64 :: Integer -> Int64#
integerToInt64 = integerToInt64

word64ToInteger :: Word64# -> Integer
word64ToInteger = word64ToInteger

int64ToInteger :: Int64# -> Integer
int64ToInteger = int64ToInteger
#endif


wordToInteger :: Word# -> Integer
wordToInteger w = smallInteger (word2Int# w)

integerToWord :: Integer -> Word#
integerToWord i = int2Word# (toInt# i)

floatFromInteger :: Integer -> Float#
floatFromInteger i = int2Float# (toInt# i)

doubleFromInteger :: Integer -> Double#
doubleFromInteger i = int2Double# (toInt# i)

divInteger :: Integer -> Integer -> Integer
x `divInteger` y
        -- Be careful NOT to overflow if we do any additional arithmetic
        -- on the arguments...  the following  previous version of this
        -- code has problems with overflow:
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#
    = if (x `gtInteger` smallInteger 0#) && (y `ltInteger` smallInteger 0#)
      then ((x `minusInteger` smallInteger 1#) `quotInteger` y) `minusInteger` smallInteger 1#
      else if (x `ltInteger` smallInteger 0#) && (y `gtInteger` smallInteger 0#)
           then ((x `plusInteger` smallInteger 1#) `quotInteger` y) `minusInteger` smallInteger 1#
           else x `quotInteger` y

modInteger :: Integer -> Integer -> Integer
x `modInteger` y
    = if (x `gtInteger` smallInteger 0#) && (y `ltInteger` smallInteger 0#) ||
         (x `ltInteger` smallInteger 0#) && (y `gtInteger` smallInteger 0#)
      then if r `neqInteger` smallInteger 0# then r `plusInteger` y else smallInteger 0#
      else r
    where
    r = x `remInteger` y

True && True = True
_    && _    = False
otherwise = True
False || False = False
_     || _     = True


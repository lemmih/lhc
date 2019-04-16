-- | Arguments and return types
module Foreign.LibFFI.Types (
    -- * Arguments
    -- ** Integral types
    argCInt,
    argCUInt,
    argCLong,
    argCULong,
    argInt,
    argInt8,
    argInt16,
    argInt32,
    argInt64,
    argWord,
    argWord8,
    argWord16,
    argWord32,
    argWord64,
    -- ** Floating point types
    argCFloat,
    argCDouble,
    -- ** Various other C types
    argCSize,
    argCTime,
    argCChar,
    argCUChar,
    argCWchar,
    argPtr,
    argFunPtr,
    -- ** Strings
    argString,
    argByteString,
    argConstByteString,
    -- * Return types
    -- ** Integral types
    retVoid,
    retCInt,
    retCUInt,
    retCLong,
    retCULong,
    retInt,
    retInt8,
    retInt16,
    retInt32,
    retInt64,
    retWord,
    retWord8,
    retWord16,
    retWord32,
    retWord64,
    -- ** Floating point types
    retCFloat,
    retCDouble,
    -- ** Various other C types
    retCSize,
    retCTime,
    retCChar,
    retCUChar,
    retCWchar,
    retPtr,
    retFunPtr,
    -- ** Strings
    retCString,
    retString,
    retByteString,
    retMallocByteString
    ) where

import Control.Monad
import Data.List
import Data.Char
import Data.Int
import Data.Word

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import Foreign.LibFFI.Base
import Foreign.LibFFI.FFITypes

argCInt     :: CInt -> Arg
argCInt     = mkStorableArg ffi_type_sint
argCUInt    :: CUInt -> Arg
argCUInt    = mkStorableArg ffi_type_uint
argCLong    :: CLong -> Arg
argCLong    = mkStorableArg ffi_type_slong
argCULong   :: CULong -> Arg
argCULong   = mkStorableArg ffi_type_ulong

-- | Note that on e.g. x86_64, Int \/= CInt
argInt      :: Int -> Arg
argInt      = mkStorableArg ffi_type_hs_int
argInt8     :: Int8 -> Arg
argInt8     = mkStorableArg ffi_type_sint8
argInt16    :: Int16 -> Arg
argInt16    = mkStorableArg ffi_type_sint16
argInt32    :: Int32 -> Arg
argInt32    = mkStorableArg ffi_type_sint32
argInt64    :: Int64 -> Arg
argInt64    = mkStorableArg ffi_type_sint64

argWord     :: Word -> Arg
argWord     = mkStorableArg ffi_type_hs_word
argWord8    :: Word8 -> Arg
argWord8    = mkStorableArg ffi_type_uint8
argWord16   :: Word16 -> Arg
argWord16   = mkStorableArg ffi_type_uint16
argWord32   :: Word32 -> Arg
argWord32   = mkStorableArg ffi_type_uint32
argWord64   :: Word64 -> Arg
argWord64   = mkStorableArg ffi_type_uint64

argCFloat   :: CFloat -> Arg
argCFloat   = mkStorableArg ffi_type_float
argCDouble  :: CDouble -> Arg
argCDouble  = mkStorableArg ffi_type_double

argCSize    :: CSize -> Arg
argCSize    = mkStorableArg ffi_type_size
argCTime    :: CTime -> Arg
argCTime    = mkStorableArg ffi_type_size

argCChar    :: CChar -> Arg
argCChar    = mkStorableArg ffi_type_schar
argCUChar   :: CUChar -> Arg
argCUChar   = mkStorableArg ffi_type_uchar

argCWchar   :: CWchar -> Arg
argCWchar   = mkStorableArg ffi_type_schar

argPtr      :: Ptr a -> Arg
argPtr      = mkStorableArg ffi_type_pointer

argFunPtr   :: FunPtr a -> Arg
argFunPtr   = mkStorableArg ffi_type_pointer

{- | The string argument is passed to C as a char * pointer, which is freed afterwards.
     The argument should not contain zero-bytes. -}
argString   :: String -> Arg
argString   = customPointerArg newCString free

-- | Like argString, but for ByteString's.
argByteString  :: BS.ByteString -> Arg
argByteString  = customPointerArg (flip BS.useAsCString return) (const $ return ())

-- | Like argByteString, but changing the string from C breaks referential transparency.
argConstByteString  :: BS.ByteString -> Arg
argConstByteString  = customPointerArg (flip BSU.unsafeUseAsCString return) (const $ return ())

retVoid     :: RetType ()
retVoid     = RetType ffi_type_void (\write -> write nullPtr >> return ())

retCInt     :: RetType CInt
retCInt     = mkStorableRetType ffi_type_sint
retCUInt    :: RetType CUInt
retCUInt    = mkStorableRetType ffi_type_uint
retCLong    :: RetType CLong
retCLong    = mkStorableRetType ffi_type_slong
retCULong   :: RetType CULong
retCULong   = mkStorableRetType ffi_type_ulong

retInt      :: RetType Int
retInt      = mkStorableRetType ffi_type_hs_int
retInt8     :: RetType Int8
retInt8     = mkStorableRetType ffi_type_sint8
retInt16    :: RetType Int16
retInt16    = mkStorableRetType ffi_type_sint16
retInt32    :: RetType Int32
retInt32    = mkStorableRetType ffi_type_sint32
retInt64    :: RetType Int64
retInt64    = mkStorableRetType ffi_type_sint64

retWord     :: RetType Word
retWord     = mkStorableRetType ffi_type_hs_word
retWord8    :: RetType Word8
retWord8    = mkStorableRetType ffi_type_uint8
retWord16   :: RetType Word16
retWord16   = mkStorableRetType ffi_type_uint16
retWord32   :: RetType Word32
retWord32   = mkStorableRetType ffi_type_uint32
retWord64   :: RetType Word64
retWord64   = mkStorableRetType ffi_type_uint64

retCFloat   :: RetType CFloat
retCFloat   = mkStorableRetType ffi_type_float
retCDouble  :: RetType CDouble
retCDouble  = mkStorableRetType ffi_type_double

retCSize    :: RetType CSize
retCSize    = mkStorableRetType ffi_type_size
retCTime    :: RetType CTime
retCTime    = mkStorableRetType ffi_type_time

retCChar    :: RetType CChar
retCChar    = mkStorableRetType ffi_type_schar
retCUChar   :: RetType CUChar
retCUChar   = mkStorableRetType ffi_type_uchar

retCWchar   :: RetType CWchar
retCWchar   = mkStorableRetType ffi_type_schar

retFunPtr   :: RetType a -> RetType (FunPtr a)
retFunPtr _ = mkStorableRetType ffi_type_pointer

retPtr      :: RetType a -> RetType (Ptr a)
retPtr _    = mkStorableRetType ffi_type_pointer

retCString          :: RetType CString
retCString          = retPtr retCChar

{- | Peek a String out of the returned char *. The char * is not freed. -}
retString           :: RetType String
retString           = withRetType peekCString (retPtr retCChar)

{- | Like retString, but for ByteString's -}
retByteString       :: RetType BS.ByteString
retByteString       = withRetType BS.packCString (retPtr retCChar)

{- | Make a ByteString out of the returned char *.
     The char * will be free(3)ed when the ByteString is garbage collected. -}
retMallocByteString :: RetType BS.ByteString
retMallocByteString = withRetType BSU.unsafePackMallocCString (retPtr retCChar)

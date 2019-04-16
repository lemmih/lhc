{-# LANGUAGE ForeignFunctionInterface #-}
{- | The pointers exported and used by the C libffi describing basic ffi types. -}
module Foreign.LibFFI.FFITypes where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import Foreign.LibFFI.Internal

foreign import ccall unsafe "&" ffi_type_void :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint8 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint8 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint16 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint16 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint32 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint32 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint64 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint64 :: Ptr CType
foreign import ccall unsafe "&" ffi_type_float  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_double :: Ptr CType
foreign import ccall unsafe "&" ffi_type_pointer :: Ptr CType

ffi_type_uchar  :: Ptr CType
ffi_type_uchar  = ffi_type_uint8

ffi_type_schar  :: Ptr CType
ffi_type_schar  = ffi_type_sint8

ffi_type_wchar  :: Ptr CType
ffi_type_wchar  = case sizeOf (undefined :: CWchar) of
                    2   -> ffi_type_sint16
                    4   -> ffi_type_sint32
                    8   -> ffi_type_sint64
                    _   -> error "ffi_type_wchar of unsupported size"

ffi_type_size   :: Ptr CType
ffi_type_size   = case sizeOf (undefined :: CSize) of
                    4   -> ffi_type_uint32
                    8   -> ffi_type_uint64
                    _   -> error "ffi_type_size of unsupported size"

ffi_type_time   :: Ptr CType
ffi_type_time   = case sizeOf (undefined :: CTime) of
                    4   -> ffi_type_sint32
                    8   -> ffi_type_sint64
                    _   -> error "ffi_type_time of unsupported size"

ffi_type_uint   :: Ptr CType
ffi_type_uint   = case sizeOf (undefined :: CUInt) of
                    4   -> ffi_type_uint32
                    8   -> ffi_type_uint64
                    _   -> error "ffi_type_uint of unsupported size"

ffi_type_sint   :: Ptr CType
ffi_type_sint   = case sizeOf (undefined :: CInt) of
                    4   -> ffi_type_sint32
                    8   -> ffi_type_sint64
                    _   -> error "ffi_type_sint of unsupported size"

ffi_type_ulong  :: Ptr CType
ffi_type_ulong  = case sizeOf (undefined :: CULong) of
                    4   -> ffi_type_uint32
                    8   -> ffi_type_uint64
                    _   -> error "ffi_type_ulong of unsupported size"

ffi_type_slong  :: Ptr CType
ffi_type_slong  = case sizeOf (undefined :: CLong) of
                    4   -> ffi_type_sint32
                    8   -> ffi_type_sint64
                    _   -> error "ffi_type_slong of unsupported size"

ffi_type_hs_int :: Ptr CType
ffi_type_hs_int = case sizeOf (undefined :: Int) of
                    4   -> ffi_type_sint32
                    8   -> ffi_type_sint64
                    _   -> error "ffi_type_hs_int: unsupported sizeOf (_ :: Int)"

ffi_type_hs_word :: Ptr CType
ffi_type_hs_word = case sizeOf (undefined :: Word) of
                    4   -> ffi_type_uint32
                    8   -> ffi_type_uint64
                    _   -> error "ffi_type_hs_word: unsupported sizeOf (_ :: Word)"

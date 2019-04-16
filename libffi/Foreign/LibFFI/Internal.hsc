{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{- | The internals of the C library libffi -}
module Foreign.LibFFI.Internal where

#include <ffi.h>

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

data CValue
data CType
data CIF

type C_ffi_status   = (#type ffi_status)
type C_ffi_abi      = (#type ffi_abi)

ffi_default_abi :: C_ffi_abi
ffi_default_abi = #const FFI_DEFAULT_ABI

ffi_ok          :: C_ffi_status
ffi_ok          = #const FFI_OK

sizeOf_cif :: Int
sizeOf_cif = #size ffi_cif

sizeOf_ffi_type :: Int
sizeOf_ffi_type = #size ffi_type

init_ffi_type   :: Ptr CType -> Ptr (Ptr CType) -> IO ()
init_ffi_type cType cTypes = do
    (#poke ffi_type, size) cType (0 :: CSize)
    (#poke ffi_type, alignment) cType (0 :: CUShort)
    (#poke ffi_type, type) cType ((#const FFI_TYPE_STRUCT) :: CUShort)
    (#poke ffi_type, elements) cType cTypes

foreign import ccall safe ffi_prep_cif
    :: Ptr CIF -> C_ffi_abi -> CUInt -> Ptr CType -> Ptr (Ptr CType) -> IO C_ffi_status

foreign import ccall safe ffi_call
    :: Ptr CIF -> FunPtr a -> Ptr CValue -> Ptr (Ptr CValue) -> IO ()

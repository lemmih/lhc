{-# LINE 1 "Foreign/LibFFI/Internal.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{- | The internals of the C library libffi -}
module Foreign.LibFFI.Internal where



import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

data CValue
data CType
data CIF

type C_ffi_status   = (Word32)
{-# LINE 18 "Foreign/LibFFI/Internal.hsc" #-}
type C_ffi_abi      = (Word32)
{-# LINE 19 "Foreign/LibFFI/Internal.hsc" #-}

ffi_default_abi :: C_ffi_abi
ffi_default_abi = 2
{-# LINE 22 "Foreign/LibFFI/Internal.hsc" #-}

ffi_ok          :: C_ffi_status
ffi_ok          = 0
{-# LINE 25 "Foreign/LibFFI/Internal.hsc" #-}

sizeOf_cif :: Int
sizeOf_cif = (32)
{-# LINE 28 "Foreign/LibFFI/Internal.hsc" #-}

sizeOf_ffi_type :: Int
sizeOf_ffi_type = (24)
{-# LINE 31 "Foreign/LibFFI/Internal.hsc" #-}

init_ffi_type   :: Ptr CType -> Ptr (Ptr CType) -> IO ()
init_ffi_type cType cTypes = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) cType (0 :: CSize)
{-# LINE 35 "Foreign/LibFFI/Internal.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) cType (0 :: CUShort)
{-# LINE 36 "Foreign/LibFFI/Internal.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 10)) cType ((13) :: CUShort)
{-# LINE 37 "Foreign/LibFFI/Internal.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) cType cTypes
{-# LINE 38 "Foreign/LibFFI/Internal.hsc" #-}

foreign import ccall safe ffi_prep_cif
    :: Ptr CIF -> C_ffi_abi -> CUInt -> Ptr CType -> Ptr (Ptr CType) -> IO C_ffi_status

foreign import ccall safe ffi_call
    :: Ptr CIF -> FunPtr a -> Ptr CValue -> Ptr (Ptr CValue) -> IO ()

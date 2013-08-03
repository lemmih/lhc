{-# LANGUAGE TypeSynonymInstances #-}
module Grin.Eval.Primitives
    ( runExternal
    , listPrimitives
    , realWorld
    ) where

import Grin.Types hiding (Value(..))
import qualified Grin.Types as Grin

import qualified Data.Map as Map

import CompactString
import Grin.Types hiding (Value(..))
import Grin.Eval.Types

import qualified Data.Map as Map
import Control.Monad.State
import Control.Exception
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C
import Foreign.Storable
import Data.Char; import Data.Word; import Data.Bits; import Data.Int

import Foreign.Marshal (copyBytes, newArray)
import System.Posix (fdWrite,Fd(..))
import System.Posix.DynamicLinker
import Foreign.LibFFI

import Grin.Eval.Methods

-- These functions are defined in the base library. I'm not sure how to deal with this properly.
runExternal :: String -> [CompValue] -> Gen CompValue
runExternal name args
    = do let returnIO v = return (Vector [realWorld, v])
         return $
            do args' <- mapM id args
               case (name, init args') of
                 ("__hscore_memcpy_dst_off", [Lit (Lint dst),Lit (Lint off),Lit (Lint src),Lit (Lint size)]) ->
                   do let dstPtr = nullPtr `plusPtr` fromIntegral (dst+off)
                          srcPtr = nullPtr `plusPtr` fromIntegral src
                      liftIO $ copyBytes dstPtr srcPtr (fromIntegral size)
                      returnIO (Lit (Lint (dst+off)))
                 ("__hscore_PrelHandle_write", [Lit (Lint fd),Lit (Lint ptr),Lit (Lint offset),Lit (Lint size)]) ->
                   do let strPtr = nullPtr `plusPtr` fromIntegral (ptr+offset)
                      str <- liftIO $ peekCStringLen (strPtr,fromIntegral size)
                      out <- liftIO $ fdWrite (Fd (fromIntegral fd)) str
                      returnIO (Lit (Lint $ fromIntegral out))
                 ("__hscore_get_errno", []) ->
                   returnIO (Lit (Lint 0))
                 ("__hscore_bufsiz", []) ->
                   returnIO (Lit (Lint 512))
                 ("fdReady", [fd,write,msecs,isSock]) ->
                   returnIO (Lit (Lint 1))
                 ("getProgArgv", [Lit (Lint argcPtr), Lit (Lint argvPtr)]) ->
                   do args <- getCommandArgs
                      liftIO $ poke (nullPtr `plusPtr` fromIntegral argcPtr) (fromIntegral (length args) :: CInt)
                      cs <- liftIO $ newArray =<< mapM newCString args
                      liftIO $ poke (nullPtr `plusPtr` fromIntegral argvPtr) cs
                      return $ Vector [Empty]
                 (name, args) ->
                   -- If we don't recognize the function, try loading it through the linker.
                   do fnPtr <- liftIO $ dlsym Default name
                      let toCArg (Lit (Lint i)) = argCInt (fromIntegral i)
                          toCArg _              = error $ "Grin.Eval.Primitive.runExternal: Unrecognized argument type."
                      ret <- liftIO $ callFFI fnPtr retCInt (map toCArg args)
                      returnIO $ Lit (Lint (fromIntegral ret))







newtype IntArg = IntArg Int
newtype CharArg = CharArg Char
newtype ArrayArg = ArrayArg [EvalValue]
newtype AnyArg = AnyArg EvalValue
newtype PtrArg = PtrArg (Ptr ())
newtype HeapArg = HeapArg HeapPointer
data RealWorld = RealWorld

data Primitive = Primitive { primName :: String, primHandle :: ([EvalValue] -> CompExpression) }


-- Primitive handlers

listPrimitives :: GlobalScope -> [(Renamed, CompFunction)]
listPrimitives globalScope
    = [ (Builtin name, primHandle (prim globalScope)) | (name, prim) <- Map.toList allPrimitives ]

allPrimitives :: Map.Map CompactString (GlobalScope -> Primitive)
allPrimitives = Map.fromList [ (fromString name, prim) | (name, prim) <- prims ]
    where prims = [ equal, gt, lt, gte, lte
                  , chrPrim, ordPrim
                  , plus, plusWord, minus, minusWord, times, timesWord, remInt, quotInt, addIntC
                  , indexCharOffAddr
                  , readInt32OffAddr, readInt8OffAddr, readAddrOffAddr
                  , writeInt8OffAddr
                  , writeCharArray
                  , touch, makeStablePtr, writeStablePtrOffAddr
                  , noDuplicate
                  , realWorldPrim, myThreadIdPrim, raisePrim
                  , catchPrim, blockAsyncExceptions, unblockAsyncExceptions
                  , newPinnedByteArray, newAlignedPinnedByteArray
                  , unsafeFreezeByteArray, byteArrayContents
                  , updatePrim, fetchPrim, evalPrim, evalApplyPrim, applyPrim
                  , newArrayPrim, readArray, writeArray
                  , narrow8Word, narrow8Int, narrow16Word, narrow16Int, narrow32Int, negateInt
                  , narrow32Word
                  , mkWeak]



-- Primitive definitions

-- FIXME: Throw an error when values of different types are compared.
equal = mkPrimitive "==#" $ binOp (==)
gt    = mkPrimitive ">#" $ binOp (>)
lt    = mkPrimitive "<#" $ binOp (<)
gte   = mkPrimitive ">=#" $ binOp (>=)
lte   = mkPrimitive "<=#" $ binOp (<=)

plus = mkPrimitive "+#" $ binIntOp (+)
plusWord = mkPrimitive "plusWord#" $ binIntOp (+)
minus = mkPrimitive "-#" $ binIntOp (-)
minusWord = mkPrimitive "minusWord#" $ binIntOp (-)
times = mkPrimitive "*#" $ binIntOp (*)
timesWord = mkPrimitive "timesWord#" $ binIntOp (*)
remInt = mkPrimitive "remInt#" $ binIntOp rem
quotInt = mkPrimitive "quotInt#" $ binIntOp quot

chrPrim = mkPrimitive "chr#" $ return $ \(CharArg c) -> noScope $ return (Lit (Lchar c))
ordPrim = mkPrimitive "ord#" $ return $ \(IntArg i) -> noScope $ return (Lit (Lint $ fromIntegral i))

addIntC = mkPrimitive "addIntC#" $
             return $ \(IntArg a) (IntArg b) ->
                let c = fromIntegral a + fromIntegral b
                    o = c `shiftR` bitSize (0::Int)
                in noScope $ return (Vector [Lit (Lint c), Lit (Lint o)])

-- |Reads 8-bit character; offset in bytes.
indexCharOffAddr
    = mkPrimitive "indexCharOffAddr#" $
      return $ \(PtrArg ptr) (IntArg nth) ->
                 noScope $ do c <- peekByteOff ptr nth :: IO Word8
                              return (Lit (Lchar (chr (fromIntegral (c::Word8)))))

readInt32OffAddr
    = mkPrimitive "readInt32OffAddr#" $
      return $ \(PtrArg ptr) (IntArg nth) RealWorld ->
           noScope $ do i <- peekElemOff (castPtr ptr) nth
                        return (Vector [realWorld, fromInt (fromIntegral (i::Int32))])

readInt8OffAddr
    = mkPrimitive "readInt8OffAddr#" $
         return $ \(PtrArg ptr) (IntArg nth) RealWorld ->
                   noScope $ do i <-  peekElemOff (castPtr ptr) nth
                                return $ Vector [realWorld, fromInt (fromIntegral (i::Int8))]

writeInt8OffAddr
    = mkPrimitive "writeInt8OffAddr#" $
      return $ \(PtrArg ptr) (IntArg nth) (IntArg elt) RealWorld ->
               noScope $ do poke (ptr `plusPtr` nth) (fromIntegral elt :: Word8)
                            return realWorld

readAddrOffAddr
    = mkPrimitive "readAddrOffAddr#" $
         return $ \(PtrArg ptr) (IntArg nth) RealWorld ->
                    noScope $ do p <- peekElemOff (castPtr ptr) nth
                                 return $ Vector [realWorld, fromPointer p]

-- |Write 8-bit character; offset in bytes.
writeCharArray
    = mkPrimitive "writeCharArray#" $
         return $ \(PtrArg ptr) (IntArg offset) (CharArg c) RealWorld ->
                    noScope $ do poke (ptr `plusPtr` offset) (fromIntegral (ord c) :: Word8)
                                 return realWorld

touch
    = mkPrimitive "touch#" $ return $ \(AnyArg _) RealWorld ->
      noScope $ return realWorld

makeStablePtr
    = mkPrimitive "makeStablePtr#" $ return $ \(AnyArg a) RealWorld ->
      noScope $ return $ Vector [realWorld, Lit (Lstring "stable pointer")]

writeStablePtrOffAddr
    = mkPrimitive "writeStablePtrOffAddr#" $ return $ \(PtrArg p) (IntArg n) (AnyArg stablePtr) RealWorld ->
      noScope $ return realWorld

noDuplicate = mkPrimitive "noDuplicate#" $ return $ \RealWorld -> noScope $ return realWorld

realWorldPrim = mkPrimitive "realWorld#" $ return $ noScope $ return realWorld

myThreadIdPrim
    = mkPrimitive "myThreadId#" $
         return $ \RealWorld ->
                     noScope $ return (Vector [realWorld, Lit (Lint 0)])

raisePrim
    = mkPrimitive "raise#" $
      return $ \(HeapArg ptr) ->
               do st <- get
                  liftIO $ throwIO (GrinException st ptr) :: CompValue

catchPrim
    = mkPrimitive "catch#" $
      do evalApply <- lookupFunction (Builtin $ fromString "evalApply")
         apply <- lookupFunction (Builtin $ fromString "apply")
         return $ \(AnyArg fn) (AnyArg handler) RealWorld ->
                  evalApply [fn, realWorld]
{- `catchComp` \val ->
                  do v <- evalApply [handler, val]
                     apply [v, realWorld]-}

blockAsyncExceptions
    = mkPrimitive "blockAsyncExceptions#" $
      do apply <- lookupFunction (Builtin $ fromString "evalApply")
         return $ \(AnyArg fn) RealWorld ->
                     apply [fn,realWorld]

unblockAsyncExceptions
    = mkPrimitive "unblockAsyncExceptions#" $
      do apply <- lookupFunction (Builtin $ fromString "evalApply")
         return $ \(AnyArg fn) RealWorld ->
                     apply [fn, realWorld]

-- |Create a mutable byte array that the GC guarantees not to move.
newPinnedByteArray
    = mkPrimitive "newPinnedByteArray#" $
         return $ \(IntArg size) RealWorld ->
                    noScope $ do ptr <- mallocBytes size
                                 return (Vector [realWorld, fromPointer ptr])

newAlignedPinnedByteArray
    = mkPrimitive "newAlignedPinnedByteArray#" $
         return $ \(IntArg size) (IntArg alignment) RealWorld ->
                    noScope $ do ptr <- mallocBytes (size + alignment)
                                 return (Vector [realWorld, fromPointer $ alignPtr ptr alignment])

unsafeFreezeByteArray
    = mkPrimitive "unsafeFreezeByteArray#" $
         return $ \(PtrArg ptr) RealWorld ->
                     noScope $ return (Vector [realWorld, fromPointer ptr])

byteArrayContents
    = mkPrimitive "byteArrayContents#" $ return $ \(PtrArg ptr) ->
      noScope $ return (fromPointer ptr)

updatePrim
    = mkPrimitive "update" $ return $ \(HeapArg ptr) (AnyArg val) ->
      do updateValue ptr val
         return Empty

fetchPrim
    = mkPrimitive "fetch" $ return $ \(HeapArg ptr) ->
      do fetch ptr

evalPrim
    = mkPrimitive "eval" $ return $ \(AnyArg arg) ->
      runEvalPrimitive arg

evalApplyPrim
    = mkPrimitive "evalApply" $ return $ \(AnyArg fnPtr) (AnyArg arg) ->
      do fn <- runEvalPrimitive fnPtr
         case fn of
              FNode name fn 1 args -> fn (args ++ [arg])
              FNode name fn 0 args -> error "apply: over application?"
              FNode name fn n args -> return $ FNode name fn (n-1) (args ++ [arg])
              CNode name 0 args -> error "apply: over application?"
              CNode name n args -> return $ CNode name (n-1) (args ++ [arg])
              _ -> error $ "weird apply: " ++ show fn

applyPrim
    = mkPrimitive "apply" $ return $ \(AnyArg fn) (AnyArg arg) ->
      do case fn of
           FNode name fn 1 args -> fn (args ++ [arg])
           FNode name fn 0 args -> error "apply: over application?"
           FNode name fn n args -> return $ FNode name fn (n-1) (args ++ [arg])
           CNode name 0 args -> error "apply: over application?"
           CNode name n args -> return $ CNode name (n-1) (args ++ [arg])
           _ -> error $ "weird apply: " ++ show fn

newArrayPrim
    = mkPrimitive "newArray#" $
         return $ \(IntArg len) (AnyArg elt) RealWorld ->
                    do ptr <- storeValue (Array $ replicate len elt)
                       return $ Vector [realWorld, HeapPointer ptr]

readArray
    = mkPrimitive "readArray#" $
         return $ \(HeapArg ptr) (IntArg idx) RealWorld ->
                    do Array arr <- fetch ptr
                       return $ Vector [realWorld, arr!!idx]

writeArray
    = mkPrimitive "writeArray#" $ return $ \(HeapArg ptr) (IntArg idx) (AnyArg val) RealWorld ->
      do Array arr <- fetch ptr
         let (before,after) = splitAt idx arr
         updateValue ptr (Array (before ++ [val] ++ drop 1 after))
         return realWorld

-- |Create @MutVar\#@ with specified initial value in specified state thread.
{-
newMutVar
    = mkPrimitive "newMutVar#" $
         return $ \(AnyArg val) RealWorld ->
                    do ptr <- storeValue val
                       return $ Vector [realWorld, HeapPointer ptr]

-- |Write contents of @MutVar\#@.
writeMutVar
    = mkPrimitive "writeMutVar#" $ return $ \(HeapArg ptr) (AnyArg val) RealWorld ->
      do updateValue ptr val
         return realWorld

-- |Read contents of @MutVar\#@. Result is not yet evaluated.
readMutVar
    = mkPrimitive "readMutVar#" $
         return $ \(HeapArg ptr) RealWorld ->
                  do val <- fetch ptr
                     return (Vector [realWorld, val])
-}
-- Dummy primitive
mkWeak = mkPrimitive "mkWeak#" $
            return $ \(AnyArg key) (AnyArg val) (AnyArg finalizer) RealWorld ->
                       noScope $ return (Vector [realWorld, Empty])

fromInt32 i = fromInt (fromIntegral (fromIntegral i::Int32)::Int)
fromWord32 i = fromInt (fromIntegral (fromIntegral i::Word32)::Int)
fromInt16 i = fromInt (fromIntegral (fromIntegral i::Int16)::Int)
fromWord16 i = fromInt (fromIntegral (fromIntegral i::Word16)::Int)
fromInt8 i = fromInt (fromIntegral (fromIntegral i::Int8)::Int)
fromWord8 i = fromInt (fromIntegral (fromIntegral i::Word8)::Int)

narrow32Int
    = mkPrimitive "narrow32Int#" $ return $ \(IntArg i) -> noScope $ return (fromInt32 i)
narrow32Word
    = mkPrimitive "narrow32Word#" $ return $ \(IntArg i) -> noScope $ return (fromWord32 i)

narrow16Int
    = mkPrimitive "narrow16Int#" $ return $ \(IntArg i) -> noScope $ return (fromInt16 i)
narrow16Word
    = mkPrimitive "narrow16Word#" $ return $ \(IntArg i) -> noScope $ return (fromWord16 i)

narrow8Word
    = mkPrimitive "narrow8Word#" $ return $ \(IntArg i) -> noScope $ return (fromInt8 i)
narrow8Int
    = mkPrimitive "narrow8Int#" $ return $ \(IntArg i) -> noScope $ return (fromWord8 i)

negateInt
    = mkPrimitive "negateInt#" $ return $ \(IntArg i) -> noScope $ return (fromInt (negate i))





-- Primitive helpers


noScope :: IO EvalValue -> CompValue
noScope = liftIO


runEvalPrimitive :: EvalValue -> CompValue
runEvalPrimitive (HeapPointer ptr)
    = worker =<< fetch ptr
    where worker orig@(FNode name fn 0 args)
              = do --liftIO $ putStrLn $ "Running: " ++ show name ++ " " ++ show args
                   reduced <- fn args
                   updateValue ptr reduced
                   return reduced
          worker val = return val
runEvalPrimitive val = error $ "unhandled eval: " ++ show val

fromPointer :: Ptr a -> EvalValue
fromPointer ptr = Lit (Lint $ fromIntegral (minusPtr ptr nullPtr))

fromInt :: Int -> EvalValue
fromInt = Lit . Lint . fromIntegral

binOp :: (EvalValue -> EvalValue -> Bool) -> Gen (AnyArg -> AnyArg -> CompValue)
binOp fn
    = do return $ \(AnyArg a) (AnyArg b) ->
                     noScope $ if a `fn` b then return (Lit (Lint 1)) else return (Lit (Lint 0))


binIntOp :: (Int -> Int -> Int) -> Gen (IntArg -> IntArg -> CompValue)
binIntOp fn
    = return $ \(IntArg a) (IntArg b) -> noScope $ return (Lit (Lint $ fromIntegral (fn a b)))

realWorld :: EvalValue
realWorld = Empty





-- Mechanism for the primitives

class IsPrimitive a where toPrimHandle :: String -> a -> [EvalValue] -> CompValue

instance (IsPrimitive b, FromArg a) => IsPrimitive (a -> b) where
    toPrimHandle name fn (x:xs) = do val <- liftIO $ fromArg x
                                     toPrimHandle name (fn val) xs
    toPrimHandle name fn [] = error $ "Grin.Eval.Primitives.toPrimHandle: Not enough arguments for: " ++ name

instance IsPrimitive (CompValue) where
    toPrimHandle name fn [] = fn
    toPrimHandle name fn _  = error $ "Grin.Eval.Primitives.toPrimHandle: Too many arguments to: " ++ name

class FromArg a where fromArg :: EvalValue -> IO a

instance FromArg AnyArg where
    fromArg = return . AnyArg
instance FromArg RealWorld where
    fromArg Empty = return RealWorld
    fromArg v     = error $ "Grin.Eval.Primitives.fromArg: Expected realWorld: " ++ show v
instance FromArg IntArg where
    fromArg (Lit (Lint i))  = return (IntArg $ fromIntegral i)
    fromArg (Lit (Lchar c)) = return (IntArg $ ord c)
    fromArg v               = error $ "Grin.Eval.Primitives.fromArg: Expected integer: " ++ show v
instance FromArg CharArg where
    fromArg (Lit (Lchar c)) = return (CharArg c)
    fromArg (Lit (Lint i))  = return (CharArg (chr $ fromIntegral i))
    fromArg v               = error $ "Grin.Eval.Primitives.fromArg: Expected char: " ++ show v
instance FromArg ArrayArg where
    fromArg (Array arr) = return (ArrayArg arr)
    fromArg v           = error $ "Grin.Eval.Primitives.fromArg: Expected array: " ++ show v
instance FromArg HeapArg where
    fromArg (HeapPointer ptr) = return (HeapArg ptr)
    fromArg v                 = error $ "Grin.Eval.Primitives.fromArg: Expected heap pointer: " ++ show v
instance FromArg PtrArg where
    fromArg (Lit (Lstring str)) = do ptr <- liftIO $ newCString str
                                     return $ PtrArg (castPtr ptr)
    fromArg (Lit (Lint ptr))    = return $ PtrArg (nullPtr `plusPtr` fromIntegral ptr)
    fromArg v = error $ "Grin.Eval.Primitives.fromArg: Expected pointer: " ++ show v


mkPrimitive :: IsPrimitive a => String -> Gen a -> (String, GlobalScope -> Primitive)
mkPrimitive name fn
    = (name, \global -> Primitive { primName = name
                                  , primHandle = toPrimHandle name (fn global) })





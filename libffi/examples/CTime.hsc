{-# LANGUAGE RankNTypes #-}
module Main where

#include <time.h>

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.LibFFI
import Foreign.LibFFI.Base
import Foreign.LibFFI.FFITypes
import System.Posix.DynamicLinker

withDLCall :: String -> ((forall a. String -> RetType a -> [Arg] -> IO a) -> IO b) -> IO b
withDLCall lib f = do
    withDL lib [RTLD_NOW] $ \dl ->
        f $ \sym ret args -> do
                    p <- dlsym dl sym
                    callFFI p ret args

main = do
    withDLCall "" $ \call -> do
        t <- call "time" retCTime [argPtr nullPtr]

        with t $ \t_p -> do
                    tm_p <- call "localtime" (retPtr retVoid) [argPtr t_p]
                    tm <- peek (castPtr tm_p :: Ptr TM)
                    t' <- call "mktime" retCTime [argPtr tm_p]
                    print t
                    print tm
                    print t'

    withDLCall "./mytime.so" $ \call -> do
        t <- call "time" retCTime [argPtr nullPtr]

        -- struct tm actually has a few architecture dependent "hidden" fields...
        (argTM, retTM, freeTMType)
            <- newStorableStructArgRet $ replicate 9 ffi_type_sint ++ [ffi_type_slong, ffi_type_pointer]
            :: IO (TM -> Arg, RetType TM, IO ())

        tm <- call "mylocaltime" retTM [argCTime t]
        t' <- call "mymktime" retCTime [argTM tm]
        freeTMType
        print t
        print tm
        print t'

data TM = TM {sec, min, hour, mday, mon, year, wday, yday, isdst :: CInt}
    deriving (Eq, Show)

instance Storable TM where
        alignment _ = #{size int}
        sizeOf _ = #{size struct tm}
        peek p = do
                    sec <- #{peek struct tm, tm_sec} p
                    min <- #{peek struct tm, tm_min} p
                    hour <- #{peek struct tm, tm_hour} p
                    mday <- #{peek struct tm, tm_mday} p
                    mon <- #{peek struct tm, tm_mon} p
                    year <- #{peek struct tm, tm_year} p
                    wday <- #{peek struct tm, tm_wday} p
                    yday <- #{peek struct tm, tm_yday} p
                    isdst <- #{peek struct tm, tm_isdst} p
                    return $ TM sec min hour mday mon year wday yday isdst
        poke p (TM sec min hour mday mon year wday yday isdst) = do
                #{poke struct tm, tm_sec} p sec
                #{poke struct tm, tm_min} p min
                #{poke struct tm, tm_hour} p hour
                #{poke struct tm, tm_mday} p mday
                #{poke struct tm, tm_mon} p mon
                #{poke struct tm, tm_year} p year
                #{poke struct tm, tm_wday} p wday
                #{poke struct tm, tm_yday} p yday
                #{poke struct tm, tm_isdst} p isdst

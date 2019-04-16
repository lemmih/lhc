module Main where

import Control.Monad
import Foreign.C.Types
import Foreign.LibFFI
import System.Posix.DynamicLinker
import Numeric
import CPUTime
import Time
import Ratio
import System.Environment
import System.Exit

main = withDL "" [RTLD_NOW] $ \dl -> do
    args <- getArgs
    sz <- case args of
                [n] -> return $ (read n * 2^20) `quot` 2
                []  -> putStrLn "usage: MemSpeed megabytes-to-use" >> exitWith (ExitFailure 1)

    memset <- dlsym dl "memset"
    memcpy <- dlsym dl "memcpy"
    malloc <- dlsym dl "malloc"
    free <- dlsym dl "free"

    s <- callFFI malloc (retPtr retVoid) [argCSize sz]
    d <- callFFI malloc (retPtr retVoid) [argCSize sz]
    check sz "memcpy 1" $ callFFI memcpy retVoid [argPtr d, argPtr s, argCSize sz]
    check (10*sz) "memcpy 10" $ replicateM_ 10 $ callFFI memcpy retVoid [argPtr d, argPtr s, argCSize sz]
    callFFI free retVoid [argPtr s]
    callFFI free retVoid [argPtr d]

    p <- callFFI malloc (retPtr retVoid) [argCSize (2 * sz)]
    check (2*sz) "memset 1" $ callFFI memset retVoid [argPtr p, argCInt 97, argCSize (2 * sz)]
    check (20*sz) "memset 10" $ replicateM_ 10 $ callFFI memset retVoid [argPtr p, argCInt 97, argCSize (2 * sz)]
    callFFI free retVoid [argPtr p]

check sz s a = do
    (r, cpu, clock) <- timeIt a
    putStrLn $ s ++ ": "
                        ++ showf 2 ((fromIntegral sz / cpu) / (2 ^ 20)) ++ " mb/cpu sec  "
                        ++ showf 2 ((fromIntegral sz / clock) / (2 ^ 20)) ++ " mb/clock sec  "
    return r

type TimeIt     = (Integer, ClockTime)

timeItStart     :: IO TimeIt
timeItStart     = liftM2 (,) getCPUTime getClockTime

timeItEnd       :: TimeIt -> IO (Double, Double)
timeItEnd (startCPU, startClock) = do
    stopCPU <- getCPUTime
    stopClock <- getClockTime
    let
        cpuTime     = (fromIntegral (stopCPU - startCPU) / 10^12)
        clockTime   = (timeDiffToSec $ diffClockTimes stopClock startClock)
    return (cpuTime, clockTime)
    where
        timeDiffToSec td
            = fromIntegral (tdSec td) + fromIntegral (tdPicosec td) / 10^12

{- | @timeIt action@ executes @action@, then returns
   a tuple of its result, CPU- and wallclock-time elapsed. -}
timeIt :: IO a -> IO (a, Double, Double)
timeIt a = do
    t <- timeItStart
    r <- a
    (cpuTime, clockTime) <- timeItEnd t
    return (r, cpuTime, clockTime)

showf           :: RealFloat a => Int -> a -> String
showf n x
    | x >= 0    = ' ':s
    | otherwise = s
    where
        s       = showFFloat (Just n) x ""

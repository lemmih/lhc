{-# LANGUAGE BangPatterns #-}
module Manager where

import Text.PrettyPrint.ANSI.Leijen
import Text.Printf
import System.FilePath
import System.IO
import Data.Time

-- | A 'Step' describes a single, pure transformation over any value
data Step a = Fixpoint [Step a]
            | Single String (a -> a)

-- | A transformer takes a value and transforms it, yielding another value in the IO monad
type Transformer a = a -> IO a

-- | 'transformer' takes a series of 'Step's which describe a transformation
-- over a value and an initial value. It repeatedly applies the 'Step's that have
-- been given to it until a fixed point is found. It may also write out debugging files.
transformer :: (Eq a, Pretty a) => Maybe FilePath -> [Step a] -> Transformer a
transformer target steps firstValue
    = do (val, n) <- toFixpoint target steps firstValue
         printf "\nFound fixpoint in %d iterations.\n" (n ::Int)
         return val

toFixpoint target [] firstValue
    = return (firstValue, 0)
toFixpoint target steps firstValue
    = worker steps steps firstValue firstValue 0 0
    where worker _ _ _ endValue s n | s >= 5 = return (endValue, n)
          worker orig_steps [] startValue endValue s n
              | startValue == endValue
              = do case target of
                     Nothing -> return ()
                     Just f  -> writeFile (printf "%s_%03d" f n) (show $ pretty endValue)
                   return (endValue, n)
              | otherwise
              = do case target of
                     Nothing -> return ()
                     Just f  -> writeFile (printf "%s_%03d" f n) (show $ pretty endValue)
                   worker orig_steps orig_steps endValue endValue (s+1) (n+1)
          worker orig_steps (Single _ fn:xs) startValue intermediaryValue s n
              = do let !value = fn intermediaryValue 
                   putStr "." >> hFlush stdout
                   worker orig_steps xs startValue value s $! n
          worker orig_steps (Fixpoint steps:xs) startValue intermediaryValue s n
              = do (val, n') <- worker steps steps intermediaryValue intermediaryValue 0 n
                   worker orig_steps xs startValue val s n'

-- | A convenient timer around the 'timeItT' function.
timeIt :: String -> IO a -> IO a
timeIt msg action
    = do printf "%-40s" (msg ++ "... ")
         hFlush stdout
         (t, a) <- timeItT action
         printf " done in %6.2fs\n" t
         return a

-- We can't use the 'timeit' library since we're interested in wall clock time.
timeItT :: IO a -> IO (Double, a)
timeItT action
    = do start <- getCurrentTime
         val <- action
         end <- getCurrentTime
         return (realToFrac (diffUTCTime end start), val)

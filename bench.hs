{-# LANGUAGE GADTs #-}
module Main where

import Control.Exception
import System.Process
import System.FilePath
import System.Directory
import System.Exit
import System.IO
import Control.Monad
import Data.List
import Numeric
import Text.Printf

main :: IO ()
main = return ()

nCycles = 1
buildScript = "." </> "build.sh"

-- programs = ["BushTail"]
programs = ["Allocate", "BinTree", "Bush", "BushTail", "Imbalanced",
            "MemBench", "PowerSet"]

data Style = Baseline | TailCopy | TailCompact
  deriving (Show)

styles = [Baseline, TailCopy, TailCompact]

type Millisecond = Int
type Bytes = Int
type MutTime = Millisecond
type GCTime = Millisecond

type Collections = Int
type Copied = Bytes
type Allocated = Bytes
type MaxHeap = Bytes
type PageFaults = Int
type Cycles = Int
type Instructions = Int
type Branches = Int
type BranchMisses = Int
type LLCLoads = Int
type LLCMisses = Int

data Stat a where
  Runtime :: Stat (MutTime,GCTime)
  GCStats :: Stat (Collections, Copied, Allocated, MaxHeap)
  Perf :: Stat (PageFaults, Cycles, Instructions, Branches, BranchMisses, LLCLoads, LLCMisses)

getStats :: IO ()
getStats = do
  sanityCheck

  ------------------------------------
  -- Runtime
  putStr (replicate (length programs * length styles * nCycles) '-')
  putStr "\r"
  runtimes <- forM programs $ \prog ->
    forM styles $ \style ->
      unzip <$> replicateM nCycles (getStat prog style Runtime)
  let (muttimes, gctimes) = unzip (map unzip runtimes)
  let (mutTabbed, mutAggr) = tabulate showMilli muttimes
      (gcTabbed, gcAggr) = tabulate showMilli gctimes

  putStr $ ppTable "Mutator times:" mutTabbed

  putStr $ ppTable "GC times:" gcTabbed

  ------------------------------------
  -- GCStats
  putStr (replicate (length programs * length styles) '-')
  putStr "\r"
  runtimes <- forM programs $ \prog ->
    forM styles $ \style ->
      unzip4 <$> replicateM nCycles (getStat prog style GCStats)
  let (collections, copied, allocated, maxheap) = unzip4 (map unzip4 runtimes)
  let (colTabbed, colAggr) = tabulate show collections
      (copiedTabbed, copiedAggr) = tabulate showBytes copied
      (allocTabbed, allocAggr) = tabulate showBytes allocated
      (heapTabbed, heapAggr) = tabulate showBytes maxheap

  putStr $ ppTable "Collections:" colTabbed
  putStr $ ppTable "Copied:" copiedTabbed
  putStr $ ppTable "Allocated:" allocTabbed
  putStr $ ppTable "Heap size:" heapTabbed
  return ()

getStat :: String -> Style -> Stat a -> IO a
getStat prog style stat = do
  putStr "*\b"
  -- build prog
  hasProg <- doesFileExist dstFile
  unless hasProg $
    mustSucceed "build" =<< waitForProcess =<< spawnProcess buildScript [srcFile]
  case stat of
    Runtime -> do
      -- run: ./prog +RTS -s (args from style)
      (stdout,stdin,stderr,p) <- runInteractiveProcess dstFile (["+RTS", "-s"] ++ styleArgs) Nothing Nothing
      hClose stdout
      output <- hGetContents stderr
      mustSucceed prog =<< waitForProcess p
      putStr "."
      -- collect stderr
      -- look for Realtime and "GC Realtime"
      case map words $ lines output of
        [["Realtime:",mutTime]
         ,_
         ,["GC", "Realtime:",gcTime]
         ,_] -> do
          return (readSeconds mutTime, readSeconds gcTime)
        _ -> do
          hPutStrLn stderr $ "Invalid output:\n" ++ output
          exitWith (ExitFailure 1)
    GCStats -> do
      -- run: ./prog +RTS --gc-stats --machine-readable (args from style)
      (stdout,stdin,stderr,p) <- runInteractiveProcess dstFile (["+RTS", "--gc-stats", "--machine-readable"] ++ styleArgs) Nothing Nothing
      hClose stdout
      output <- hGetContents stderr
      evaluate (length output)
      mustSucceed prog =<< waitForProcess p
      putStr "."
      case map words $ lines output of
        [  "Collections:", collections] :
          ["Copied:", copied, "b"] :
          ["Allocated:", allocated, "b"] :
          ["Max", "heap:", maxheap, "b"] :
          _ -> return (read collections, read copied, read allocated, read maxheap)
        _   -> do
          hPutStrLn stderr $ "Invalid output:\n" ++ output
          exitWith (ExitFailure 1)
  where
    srcFile = "examples" </> prog <.> "hs"
    dstFile = "." </> prog
    styleArgs =
      case style of
        Baseline -> []
        TailCopy -> ["--tail-copy"]
        TailCompact -> ["--tail-copy", "--tail-compact"]

readSeconds :: String -> Millisecond
readSeconds inp =
  case reads inp of
    [(float,"s")] -> round (float*1000)
    _             -> error $ "bad parse: " ++ inp

ppTable :: String -> [[String]] -> String
ppTable caption table = unlines $
  ["", caption] ++
  [ppRow $ "":map show styles] ++
  [ ppRow $ prog : progValues
  | (prog, progValues) <- zip programs table ]

ppRow :: [String] -> String
ppRow = concatMap (printf "%-20s")

-- Compute values compared against baseline.
-- Compute min, max, and mean
tabulate :: (Int -> String) -> [[[Int]]] -> ([[String]], [[String]])
tabulate ppFn input = (map row input,[])
  where
    row (baseline:rest) = showPrimary baseline : map (showRelative (median baseline)) rest
    showRelative baseMed lst = diff ("% (±" ++ dev "%)")
      where
        diff = showFFloat (Just 1) (fromIntegral (med-baseMed) / fromIntegral baseMed * 100)
        med = median lst
        dev = showFFloat (Just 1) (deviation lst med*100)
    showPrimary lst = ppFn med ++ " (±" ++ dev "%)"
      where
        med = median lst
        dev = showFFloat (Just 1) (deviation lst med*100)

showMilli :: Millisecond -> String
showMilli usec = showFFloat (Just 1) (fromIntegral usec / 1000) "s"

showBytes :: Bytes -> String
showBytes bs
  | bs < 1024^1 = shows bs " b"
  | bs < 1024^2 = unit 1 " kb"
  | bs < 1024^3 = unit 2 " mb"
  | otherwise   = unit 3 " gb"
  where
    unit n label = showFFloat (Just 1) (fromIntegral bs / (1024^n)) label

median :: [Int] -> Int
median lst'
  | odd len = lst !! (len `div` 2)
  | otherwise = (lst !! (len `div` 2) + lst !! (len `div` 2 - 1)) `div` 2
  where
    lst = sort lst'
    len = length lst'

deviation :: [Int] -> Int -> Double
deviation lst target = maximum
  [ (fromIntegral (abs (e-target)) / fromIntegral target)
  | e <- lst ]

sanityCheck :: IO ()
sanityCheck = do
    mustSucceed "perf" =<< system "perf stat true"
    hasBuild <- doesFileExist buildScript
    unless hasBuild $ do
      hPutStrLn stderr $ "Missing "++buildScript++" script"

mustSucceed _ ExitSuccess = return ()
mustSucceed cmd code = do
  hPutStrLn stderr $ "Command failed: " ++ cmd
  exitWith code

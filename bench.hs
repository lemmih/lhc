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

nCycles = 10
buildScript = "." </> "build.sh"

-- programs = ["Bush"]
programs = ["Allocate", "BinTree", "Bush", "BushTail", "Imbalanced",
            "MemBench", "PowerSet"]

data Style = Baseline | TailCopy | TailCompact

instance Show Style where
  show Baseline = "\\textbf{Baseline}"
  show TailCopy = "\\textbf{Tail-copy}"
  show TailCompact = "\\textbf{Tail-compact}"

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

instance Show (Stat a) where
  show Runtime = "Runtime"
  show GCStats = "GCStats"
  show Perf = "Perf"

getStats :: IO ()
getStats = do
  -- sanityCheck

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
  putStr $ ppAggregates mutAggr

  putStr $ ppTable "GC times:" gcTabbed
  putStr $ ppAggregates gcAggr

  ------------------------------------
  -- GCStats
  -- putStr (replicate (length programs * length styles) '-')
  -- putStr "\r"
  -- runtimes <- forM programs $ \prog ->
  --   forM styles $ \style ->
  --     unzip4 <$> replicateM 1 (getStat prog style GCStats)
  -- let (collections, copied, allocated, maxheap) = unzip4 (map unzip4 runtimes)
  -- let (colTabbed, colAggr) = tabulate show collections
  --     (copiedTabbed, copiedAggr) = tabulate showBytes copied
  --     (allocTabbed, allocAggr) = tabulate showBytes allocated
  --     (heapTabbed, heapAggr) = tabulate showBytes maxheap
  --
  -- putStr $ ppTable "Collections:" colTabbed
  -- putStr $ ppAggregates colAggr
  -- putStr $ ppTable "Copied:" copiedTabbed
  -- putStr $ ppAggregates copiedAggr
  -- putStr $ ppTable "Allocated:" allocTabbed
  -- putStr $ ppAggregates allocAggr
  -- putStr $ ppTable "Heap size:" heapTabbed
  -- putStr $ ppAggregates heapAggr

  ------------------------------------
  -- Perf
  -- putStr (replicate (length programs * length styles * nCycles) '-')
  -- putStr "\r"
  -- runtimes <- forM programs $ \prog ->
  --   forM styles $ \style ->
  --     unzip7 <$> replicateM nCycles (getStat prog style Perf)
  -- let (pageFaults, cycles, instructions, branches, branchMisses, llcLoads, llcMisses) =
  --       unzip7 (map unzip7 runtimes)
  -- let (pageTabbed, pageAggr) = tabulate showCount pageFaults
  --     (cyclesTabbed, cyclesAggr) = tabulate showCount cycles
  --     (instructionsTabbed, instructionsAggr) = tabulate showCount instructions
  --     (branchesTabbed, branchesAggr) = tabulate showCount branches
  --     (branchMissesTabbed, branchMissesAggr) = tabulate showCount branchMisses
  --     (llcLoadsTabbed, llcLoadsAggr) = tabulate showCount llcLoads
  --     (llcMissesTabbed, llcMissesAggr) = tabulate showCount llcMisses
  --
  -- putStr $ ppTable "Page faults:" pageTabbed
  -- putStr $ ppAggregates pageAggr
  -- putStr $ ppTable "Cycles:" cyclesTabbed
  -- putStr $ ppAggregates cyclesAggr
  -- putStr $ ppTable "Instructions:" instructionsTabbed
  -- putStr $ ppAggregates instructionsAggr
  -- putStr $ ppTable "Branches:" branchesTabbed
  -- putStr $ ppAggregates branchesAggr
  -- putStr $ ppTable "Branch Misses:" branchMissesTabbed
  -- putStr $ ppAggregates branchMissesAggr
  -- putStr $ ppTable "LLC Loads:" llcLoadsTabbed
  -- putStr $ ppAggregates llcLoadsAggr
  -- putStr $ ppTable "LLC Load Misses:" llcMissesTabbed
  -- putStr $ ppAggregates llcMissesAggr

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
      (_stdout,_stdin,_stderr,p) <- runInteractiveProcess dstFile (["+RTS", "-s"] ++ styleArgs) Nothing Nothing
      hClose _stdout
      output <- hGetContents _stderr
      evaluate (length output)
      -- putStrLn output
      hClose _stderr
      mustSucceed (show (prog,style,stat)) =<< waitForProcess p
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
      (_stdout,_stdin,_stderr,p) <- runInteractiveProcess dstFile (["+RTS", "--gc-stats", "--machine-readable"] ++ styleArgs) Nothing Nothing
      hClose _stdout
      output <- hGetContents _stderr
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
    Perf -> do
      (_stdout,_stdin,_stderr,p) <- runInteractiveProcess "perf" (perfArgs ++ [dstFile, "+RTS"] ++ styleArgs) Nothing Nothing
      hClose _stdout
      output <- hGetContents _stderr
      evaluate (length output)
      mustSucceed "perf" =<< waitForProcess p
      putStr "."
      case map words $ lines output of
        [ (pageFaults : "page-faults" : _),
          (cycles : "cycles" : _),
          (instructions : "instructions" : _),
          ( branches : "branches" : _),
          ( branchMisses : "branch-misses" : _),
          ( llcLoads: "LLC-loads" : _),
          ( llcLoadMisses : "LLC-load-misses" : _)
          ] ->
          return (read pageFaults, read cycles, read instructions, read branches
                  , read branchMisses, read llcLoads, read llcLoadMisses)
        _ -> do
          hPutStrLn stderr $ "Invalid output:\n" ++ output
          exitWith (ExitFailure 1)
  where
    srcFile = "examples" </> prog <.> "hs"
    dstFile = "." </> prog
    perfArgs = ["stat","--event=page-faults,cycles,instructions,branches,branch-misses,LLC-loads,LLC-load-misses","--field-separator", " "]
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

{-
\hline
\multicolumn{4}{|c|}{Mutator runtime}\\
\hline
Name & Baseline & Tail-copy & Tail-compact \\

\hline
Allocate &                1.6s (±0.4\%)  &          +0.1\% (±2.9\%) &          +4.5\% (±0.9\%) \\
BinTree  &                1.8s (±+0.8\%) &          -0.5\% (±1.5\%) &          -2.8\% (±1.0\%) \\
Bush       &              2.4s (±0.4\%) &           -0.3\% (±0.3\%) &          -0.4\% (±0.2\%) \\
BushTail   &              3.6s (±0.3\%) &           -0.1\% (±0.2\%) &          -0.2\% (±0.3\%) \\
Imbalanced &              1.7s (±0.4\%) &           -0.7\% (±0.9\%) &          -0.4\% (±0.1\%) \\
MemBench   &              1.7s (±0.7\%) &           -0.3\% (±0.2\%) &          -0.5\% (±0.2\%) \\
PowerSet   &              1.1s (±0.5\%) &           -0.5\% (±0.5\%) &          -0.6\% (±0.3\%) \\
\hline
-}
ppTable :: String -> [[String]] -> String
ppTable caption table = unlines $
  ["","","", "\\hline","\\multicolumn{4}{|c|}{"++caption++"}\\\\","\\hline"] ++
  [intercalate "&" ("\\textbf{Name}":map show styles) ++ "\\\\" ] ++
  [ intercalate "&" (map (pad 15) (prog : progValues)) ++ "\\\\"
  | (prog, progValues) <- zip programs table ] ++
  ["\\hline"]

pad :: Int -> String -> String
pad n str = str ++ replicate (n-length str) ' '

{-
\hline
\multicolumn{2}{|l|}{Min} &  -0.7\% &                   -2.8\% \\
\multicolumn{2}{|l|}{Max} &   +0.1\% &                   +4.5\% \\
\multicolumn{2}{|l|}{Mean} &   -0.3\% &                   -0.1\% \\
\hline
-}
ppAggregates :: [[String]] -> String
ppAggregates aggr = unlines $
  [ "\\hline" ] ++
  [ intercalate "&" (("\\multicolumn{2}{|l|}{"++key++"}"):row) ++ "\\\\"
  | (key:row) <- aggr ] ++
  [ "\\hline" ]
  -- replicate (4*25) '-' ++ "\n" ++ unlines (map ppRow table)

ppRow :: [String] -> String
ppRow = concatMap (printf "%-25s")

-- Compute values compared against baseline.
-- Compute min, max, and mean
tabulate :: (Int -> String) -> [[[Int]]] -> ([[String]], [[String]])
tabulate ppFn input = (map row input, transpose $ ["Min", "Max", "Mean"] : map column relColumns)
  where
    relColumns = transpose
      [ [ fromIntegral (median cell-baseMed) / fromIntegral baseMed
        | let baseMed = median baseline
        , cell <- rest ]
      | (baseline:rest) <- input ]
    column cs = map showPercent [minimum cs,maximum cs,sum cs / (fromIntegral $ length cs)]
    row (baseline:rest) = showPrimary baseline : map (showRelative (median baseline)) rest
    showRelative baseMed lst = diff -- ++ (" (±" ++ dev ++ ")")
      where
        diff = showPercent' (fromIntegral (med-baseMed) / fromIntegral baseMed)
        med = median lst
        dev = showPercent (deviation lst med)
    showPrimary lst = ppFn med -- ++ " (±" ++ dev ++ ")"
      where
        med = median lst
        dev = showPercent (deviation lst med)

showPercent' f =
  (if f<0 then "" else "+") ++ showFFloat (Just 1) (f*100) "\\%"

showPercent f = showFFloat (Just 1) (f*100) "\\%"


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

showCount :: Bytes -> String
showCount bs
  | bs < 1000^1 = shows bs ""
  | bs < 1000^2 = unit 1 " thousand"
  | bs < 1000^3 = unit 2 " million"
  | otherwise   = unit 3 " billion"
  where
    unit n label = showFFloat (Just 1) (fromIntegral bs / (1000^n)) label

median :: [Int] -> Int
median lst'
  | odd len = lst !! (len `div` 2)
  | otherwise = (lst !! (len `div` 2) + lst !! (len `div` 2 - 1)) `div` 2
  where
    lst = sort lst'
    len = length lst'

geomean :: [Int] -> Int
geomean lst = round $ fromIntegral (product lst) ** (recip $ fromIntegral $ length lst)

deviation :: [Int] -> Int -> Double
deviation lst target = maximum
  [ (fromIntegral (abs (e-target)) / fromIntegral target)
  | e <- lst ]

sanityCheck :: IO ()
sanityCheck = do
  (code, _, _) <- readProcessWithExitCode "perf" ["stat", "true"] ""
  mustSucceed "perf" code
  hasBuild <- doesFileExist buildScript
  unless hasBuild $ do
    hPutStrLn stderr $ "Missing "++buildScript++" script"

mustSucceed _ ExitSuccess = return ()
mustSucceed cmd code = do
  hPutStrLn stderr $ "Command failed: " ++ cmd
  exitWith code

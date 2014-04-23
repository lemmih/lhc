module Main (main) where

import Control.Applicative
import System.Console.CmdTheLine

import Data.Bedrock.Compile

doParse :: [FilePath] -> Bool -> IO ()
doParse _ _ = return ()

parseTerm :: Term (IO ())
parseTerm = doParse <$> inputFiles <*> doValidate
  where
    inputFiles = filesExist $ nonEmpty $ posAny [] posInfo
        { posName = "INPUT"
        , posDoc  = "Bedrock files to parse."}
    doValidate = value . flag $ (optInfo ["validate","v"])
        { optDoc = "Enable source validation." }

parseInfo :: TermInfo
parseInfo = defTI
    { termName = "parse"
    , termDoc = "Parse and pretty-print bedrock source files." }


doRun :: [FilePath] -> IO ()
doRun ~[path] = do
    compileFromFile path

runTerm :: Term (IO ())
runTerm = doRun <$> inputFiles
  where
    inputFiles = filesExist $ nonEmpty $ posAny [] posInfo
        { posName = "INPUT"
        , posDoc  = "Bedrock program files."}

runInfo :: TermInfo
runInfo = defTI
    { termName = "run"
    , termDoc  = "Compile and run bedrock program." }


doLower :: IO ()
doLower = return ()

lowerTerm :: Term (IO ())
lowerTerm = pure doLower

lowerInfo :: TermInfo
lowerInfo = defTI
    { termName = "lower"
    , termDoc  = "Remove eval/apply, catch/throw, alloc." }


main :: IO ()
main = runChoice (noCommand, defTI{ termName = "bedrock"}) commands
  where
    noCommand = ret (pure $ helpFail Plain Nothing)
    commands =
        [ (parseTerm, parseInfo)
        , (runTerm, runInfo)
        , (lowerTerm, lowerInfo) ]


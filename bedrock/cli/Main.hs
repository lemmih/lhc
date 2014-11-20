module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Options.Applicative
import           System.FilePath
import           System.Process
import Text.Printf

import           Data.Bedrock.Compile

-- doParse :: [FilePath] -> Bool -> IO ()
-- doParse _ _ = return ()

-- parseTerm :: Term (IO ())
-- parseTerm = doParse <$> inputFiles <*> doValidate
--   where
--     inputFiles = filesExist $ nonEmpty $ posAny [] posInfo
--         { posName = "INPUT"
--         , posDoc  = "Bedrock files to parse."}
--     doValidate = value . flag $ (optInfo ["validate","v"])
--         { optDoc = "Enable source validation." }

-- parseInfo :: TermInfo
-- parseInfo = defTI
--     { termName = "parse"
--     , termDoc = "Parse and pretty-print bedrock source files." }



-- keepIntermediateFiles :: Term KeepIntermediateFiles
-- keepIntermediateFiles = value . flag $
--     (optInfo ["keep-intermediate-files","k"])
--     { optDoc = "Keep interdiate files" }

-- verbose :: Term Verbose
-- verbose = value . flag $
--     (optInfo ["verbose", "v"])
--     { optDoc = "Verbose" }

-- doRun :: KeepIntermediateFiles -> Verbose -> [FilePath] -> IO ()
-- doRun keepIntermediateFiles verbose ~[path] = do
--     compileFromFileWithOpts keepIntermediateFiles verbose path
--     pid <- spawnProcess "lli" [replaceExtension path "bc"]
--     waitForProcess pid
--     return ()

-- runTerm :: Term (IO ())
-- runTerm = doRun <$> keepIntermediateFiles <*> verbose <*> inputFiles
--   where
--     inputFiles = filesExist $ nonEmpty $ posAny [] posInfo
--         { posName = "INPUT"
--         , posDoc  = "Bedrock program files."}

-- runInfo :: TermInfo
-- runInfo = defTI
--     { termName = "run"
--     , termDoc  = "Compile and run bedrock program." }




doCompile :: KeepIntermediateFiles -> Verbose -> [FilePath] -> IO ()
doCompile keepIntermediateFiles verbose ~[path] = do
    compileFromFileWithOpts keepIntermediateFiles verbose path


-- compileTerm :: Term (IO ())
-- compileTerm = doCompile <$> keepIntermediateFiles <*> verbose <*> inputFiles
--   where
--     inputFiles = filesExist $ nonEmpty $ posAny [] posInfo
--         { posName = "INPUT"
--         , posDoc  = "Bedrock program files."}
-- compileInfo :: TermInfo
-- compileInfo = defTI
--     { termName = "compile"
--     , termDoc  = "Compile .rock -> .bc" }


main :: IO ()
main = join $ execParser $ info (helper <*> opts) idm
  where
    opts = foldr (<|>) empty
        [ version
        , hsubparser $ mempty ]

    version =
        flag'
            (printf "Bedrock compiler.")
            (long "version")

    compileCommand = command "compile" (info compile idm)
    compile =
        doCompile
            <$> pure True
            <*> switch
                 ( long "verbose"
                <> help "Whether to be verbose" )
            <*> (many $ argument str (metavar "FILE"))
-- main = runChoice (noCommand, defTI{ termName = "bedrock"}) commands
--   where
--     noCommand = ret (pure $ helpFail Plain Nothing)
--     commands =
--         [ (parseTerm, parseInfo)
--         , (runTerm, runInfo)
--         , (compileTerm, compileInfo) ]


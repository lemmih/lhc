module Data.Bedrock.Compile where

import           System.FilePath
import           Text.ParserCombinators.Parsec     (parseFromFile)
import           Data.IORef
import Text.Printf

--import           Data.Bedrock
import           Data.Bedrock.EvalApply
import           Data.Bedrock.Exceptions
import           Data.Bedrock.GlobalVariables
import           Data.Bedrock.HPT                  (ppHPTResult,runHPT)
import           Data.Bedrock.Invoke
import           Data.Bedrock.LLVM                 as LLVM
import           Data.Bedrock.Parse
import           Data.Bedrock.PrettyPrint
import           Data.Bedrock.RegisterIntroduction
import           Data.Bedrock.Rename
--import           Data.Bedrock.Simplify
import           Data.Bedrock.Storage
import           Data.Bedrock.Storage.Pluggable
import           Data.Bedrock.Storage.Fixed
import           Data.Bedrock.NodeSizing


compileFromFile :: FilePath -> IO ()
compileFromFile path = do
    ret <- parseFromFile parseModule path
    case ret of
        Left err -> print err
        Right m  -> do
            let -- Start the pipeline by making sure each identifier
                -- is globally unique. Many of the passes rely on this.
                renamed      = unique m
                hpt1         = runHPT renamed
                noLaziness   = unique $ runGen renamed (lowerEvalApply hpt1)
                noExceptions = unique $ runGen noLaziness cpsTransformation
                hpt2         = runHPT noExceptions
                -- FIXME: rename mkInvoke to lowerInvoke.
                noInvoke     = runGen noExceptions (mkInvoke hpt2)
                noUnknownSize= unique $ runGen noInvoke (lowerNodeSize hpt2)
                noNodes      = unique $ registerIntroduction noUnknownSize
                -- Hm, I don't think this step can be done after
                -- register introduction...
                -- Insert GC intrinsics for marking roots, checking the heap,
                -- initializing a GC run, etc.
                noAllocs     = unique $ runGen noNodes lowerAlloc
                -- Select the GC algorithm we want to use:
                (sm, withGCCode) = runGC noAllocs fixedGC
                -- Lower the GC intrinsics that we inserted above.
                noGC         = unique $ lowerGC withGCCode sm
                -- Lower the global registers used by the GC code
                -- into normal registers when possible.
                noGlobalRegs = unique $ lowerGlobalRegisters noGC
            n <- newIORef 0
            writeRocks n renamed "renamed"
            writeRocks n noLaziness "no-lazy"
            writeRocks n noExceptions "no-exceptions"
            ppHPTResult hpt2
            writeRocks n noInvoke "no-invoke"
            writeRocks n noUnknownSize "no-unknown-size"
            writeRocks n noNodes "no-nodes"
            writeRocks n noAllocs "no-allocs"
            writeRocks n withGCCode "with-gc-code"
            writeRocks n noGC "no-gc"
            writeRocks n noGlobalRegs "no-globals"
            print (ppModule noGlobalRegs)
            --print (ppModule m5)
            --print (ppModule m6)
            --evaluate m4
            LLVM.compile noGlobalRegs (replaceExtension path "bc")
  where
    base = takeBaseName path
    writeRocks nRef rocks tag = do
        n <- readIORef nRef
        writeIORef nRef (n+1 :: Int)
        printf "[%d] Running step %s\n" n (show tag)
        writeFile (base <.> show n <.> tag <.> "rock") (show $ ppModule rocks)


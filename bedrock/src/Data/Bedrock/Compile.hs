module Data.Bedrock.Compile where

import           System.FilePath
import           Text.ParserCombinators.Parsec     (parseFromFile)

import           Data.Bedrock.EvalApply
import           Data.Bedrock.Exceptions
import           Data.Bedrock.GlobalVariables
import           Data.Bedrock.HPT                  (ppHPTResult, runHPT)
import           Data.Bedrock.Invoke
import           Data.Bedrock.LLVM                 as LLVM
import           Data.Bedrock.Parse
import           Data.Bedrock.PrettyPrint
import           Data.Bedrock.RegisterIntroduction
import           Data.Bedrock.Rename
import           Data.Bedrock.Simplify
import           Data.Bedrock.Storage
import           Data.Bedrock.Storage.Pluggable
import           Data.Bedrock.Storage.Fixed
import           Data.Bedrock.Transform            (runGens)


compileFromFile :: FilePath -> IO ()
compileFromFile path = do
    ret <- parseFromFile parseModule path
    case ret of
        Left err -> print err
        Right m  -> do
            let m' = unique m
            --print (ppModule m')
            let hpt1 = runHPT m'
            ppHPTResult hpt1
            let m'' = unique $ runGens m'
                    [ lowerEvalApply hpt1
                    , cpsTransformation
                    -- , lowerAlloc
                    ]
            --print (ppModule m'')
            let hpt2 = runHPT m''
            ppHPTResult hpt2
            let m3 = simplify $ unique $ registerIntroduction $ runGens m''
                        [ mkInvoke hpt2
                        ]
                m4 = unique $ runGens m3 [ lowerAlloc ]
                (sm, m5) = runGC m4 fixedGC
                m6 = unique $ lowerGlobalRegisters $ lowerGC m5 sm
            --print (ppModule m3)
            print (ppModule m5)
            print (ppModule m6)
            --evaluate m4
            LLVM.compile m6 (replaceExtension path "bc")


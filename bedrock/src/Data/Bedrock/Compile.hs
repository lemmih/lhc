module Data.Bedrock.Compile where

import           Control.Monad                     (when)
import           System.FilePath
import           Text.ParserCombinators.Parsec     (parseFromFile)
import           Text.Printf

import           Data.Bedrock.EvalApply
import qualified Data.Bedrock.SimpleEvalApply as Simple
import           Data.Bedrock.Exceptions
import           Data.Bedrock.GlobalVariables
import           Data.Bedrock.HPT
-- import           Data.Bedrock.Invoke
import           Data.Bedrock.LLVM                 as LLVM
import           Data.Bedrock.Parse
import           Data.Bedrock.InlineByCost
import           Data.Bedrock.PrettyPrint
import           Data.Bedrock.RegisterIntroduction
import           Data.Bedrock.Rename
import           Data.Bedrock.Simplify
import           Data.Bedrock.Simplify.LocalDCE
import           Data.Bedrock.Simplify.DCE
import           Data.Bedrock.VoidElimination
import qualified Data.Bedrock.StackLayout as StackLayout
import           Data.Bedrock
import           Data.Bedrock.NodeSizing
import qualified Data.Bedrock.SimpleNodeSizing as Simple
import           Data.Bedrock.Storage
import           Data.Bedrock.Storage.Fixed
import           Data.Bedrock.Storage.Pluggable

-- Compile options
type KeepIntermediateFiles = Bool
type Verbose = Bool

type Pipeline = [Step]
data Step
    = String :> (Module -> Module)
    | String :?> (HPTResult -> Module -> Module)
    | PerformHPT

infixr 9 :>
infixr 9 :?>

runPipeline :: KeepIntermediateFiles -> Verbose
            -> String -> Module -> Pipeline -> IO Module
runPipeline keepIntermediateFiles verbose title m0 =
    worker hpt0 0 m0
  where
    worker _ _ m [] = return m
    worker hpt n m (step:steps) =
        case step of
            tag :> action -> do
                m' <- runAction n m tag action
                worker hpt (n+1) m' steps
            tag :?> action -> do
                m' <- runAction n m tag (action hpt)
                worker hpt (n+1) m' steps
            PerformHPT -> do
                let m' = unique m
                when verbose $
                    printf "** HPT Analysis\n"
                let hpt' = runHPT m'
                when verbose $ ppHPTResult hpt'
                worker hpt' n m' steps
    runAction n m tag action = do
        when verbose $
            printf "[%d] Running step %s\n" (n::Int) (show tag)
        let m' = action m
        when keepIntermediateFiles $
            writeFile (dstFile n tag) (show $ ppModule m')
        return m'
    hpt0 = runHPT m0
    dstFile n tag = title <.> show n <.> tag <.> "rock"

compileModule :: Module -> FilePath -> IO ()
compileModule m path = do
    result <- runPipeline True True (takeBaseName path) m stdPipeline
    LLVM.compile result (replaceExtension path "ll")

compileFromFile :: FilePath -> IO ()
compileFromFile = compileFromFileWithOpts True True

compileFromFileWithOpts :: KeepIntermediateFiles -> Verbose
                        -> FilePath -> IO ()
compileFromFileWithOpts keepIntermediateFiles verbose path = do
    ret <- parseFromFile parseModule path
    case ret of
        Left err -> print err
        Right m  -> do
            result <- runPipeline keepIntermediateFiles verbose base m stdPipeline
            LLVM.compile result (replaceExtension path "ll")
  where
    base = takeBaseName path

compileWithOpts :: KeepIntermediateFiles -> Verbose
                -> FilePath -> Module -> IO ()
compileWithOpts keepIntermediateFiles verbose path m = do
    result <- runPipeline keepIntermediateFiles verbose base m stdPipeline
    LLVM.compile result (replaceExtension path "ll")
  where
    base = takeBaseName path


stdPipeline :: Pipeline
stdPipeline =
        [ "rename"          :> simplify . unique
        , "inlined"         :> unique . simplifySteps 10
        -- , PerformHPT
        -- , "no-laziness"     :?> runGen . lowerEvalApply
        -- , "no-unknown-size" :?> runGen . lowerNodeSize
        , "no-laziness"     :> runGen Simple.lowerEvalApply
        , "no-void"         :> simplify . voidEliminate
        , "no-unknown-size" :> runGen Simple.lowerNodeSize

        , "no-nodes"        :> simplify . unique . registerIntroduction
        , "no-stack"        :> StackLayout.lower
        , "no-stack"        :> mergeAllocsModule . locallyUnique . runGen cpsTransformation
        -- , "no-invoke"       :?> runGen . lowerInvoke
        , "no-allocs"       :> locallyUnique . runGen lowerAlloc
        , "no-gc"           :> locallyUnique . lowerGC fixedGC
        , "no-globals"      :> unique . lowerGlobalRegisters
        , "pretty"          :> unique . localDCE . simplify . simplify
        -- , "pretty"          :> locallyUnique
        ]
  where
    simplifySteps 0 = deadCodeElimination
    simplifySteps n = simplifySteps (n-1) . unique . localDCE . simplify . inline
        -- [ "rename"          :> simplify . unique
        -- , PerformHPT
        -- , "no-laziness"     :?> runGen . lowerEvalApply
        -- , "no-exceptions"   :> unique . runGen cpsTransformation
        -- , PerformHPT
        -- , "no-invoke"       :?> runGen . lowerInvoke
        -- , "no-unknown-size" :?> runGen . lowerNodeSize
        -- , "no-nodes"        :> unique . registerIntroduction
        -- , "no-allocs"       :> unique . runGen lowerAlloc
        -- , "no-gc"           :> unique . lowerGC fixedGC
        -- , "no-globals"      :> unique . lowerGlobalRegisters
        -- ]

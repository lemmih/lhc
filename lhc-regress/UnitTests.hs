module UnitTests
    ( unitTests
    ) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit
import System.FilePath
import System.Directory
import System.Process
import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Char8 as B
import Control.Concurrent
import Control.Exception
import System.Exit
import System.IO

unitTests = [ testGroup "io" basicTests
            , testGroup "language" languageTests
            , testGroup "shootout" shootoutTests
            , testGroup "nofib" nofibTests
            , testGroup "Haskell2010" haskell2010Tests
            , testGroup "bugs" bugsTests
            ]


basicTests
    = [ lhcTest dir name | name <- tests ]
    where dir = ["tests","1_io","basic"]
          tests = [ "Args"
                  , "HelloWorld"
                  , "enum"
                  , "fastest_fib"
                  , "IORef"
                  , "Echo" ]

languageTests
    = [ lhcTest dir name | name <- tests ]
    where dir = ["tests","2_language"]
          tests = [ "Bounds"
                  , "CPP"
                  , "EnumEnum"
                  , "IntEnum"
                  , "IrrefutableLambda"
                  , "KindInference"
                  , "Kleisli"
                  , "PureInteger"
                  , "Laziness"
                  , "Defaulting"
                  , "NoMonomorphism" ]

shootoutTests
    = [ lhcTest dir name | name <- tests ]
    where dir = ["tests", "3_shootout"]
          tests = [ "BinaryTrees"
                  , "Mandelbrot"
                  , "SumFile"]

nofibTests
    = [ lhcTest dir name | name <- tests ] ++
      [ lhcTest (dir ++ ["spectral","calendar"]) "Calendar" ] ++
      [ lhcTest (dir ++ ["spectral","primes"]) "Primes" ]
    where dir = ["tests", "9_nofib"]
          tests = [ "digits-of-e1" ]

haskell2010Tests =
    [ lhcTest dir name | name <- tests ]
  where
    dir = ["tests","haskell2010"]
    tests = [ "HelloWorld"
            , "SimplePatternMatch"
            , "DeepPatternMatch"
            , "Tuples"
            , "ArithmeticSequences"
            , "ListComprehensions" ]

bugsTests
    = [ lhcTest dir name | name <- tests ]
    where dir = ["tests", "bugs"]
          tests = ["ImportZeal"
                  ,"Parsing1"
                  ,"RayT"
                  ,"Qualify1"
                  ,"Recursive2"
                  ,"UnpackedPoly"
                  ,"Exceptions1"]







------------------------------------------------------------------------------
-- Framework code

lhcTest path name
    = testCase name $
      do let testFile = joinPath path </> name <.> "hs"
         exist <- doesFileExist testFile
         unless exist $ fail "Cannot find test file."
         removeFile (dropExtension testFile) `mplus` return ()
         args <- B.readFile (replaceExtension testFile "args") `mplus` return B.empty
         input <- B.readFile (replaceExtension testFile "stdin") `mplus` return B.empty
         expectedOutput <- B.readFile (replaceExtension testFile "expected.stdout") `mplus` return B.empty
         mustfail <- doesFileExist (replaceExtension testFile "mustfail")
         handleFailures mustfail $ do
           execProcess "lhc" ["build", testFile] B.empty
           (_,output,_) <- execProcess (dropExtension testFile) (words $ B.unpack args) input
           let failed = output /= expectedOutput
           when failed $
             fail $ unlines [ "Program result doesn't match expected output."
                            , "Program output:"
                            , take 100 (show (B.unpack output))
                            , "Expected output:"
                            , take 100 (show (B.unpack expectedOutput)) ]

handleFailures False cmd = cmd
handleFailures True cmd
    = do e <- try cmd :: IO (Either SomeException ())
         case e of
           Right () -> fail $ "Program succeded unexpectantly."
           Left e   -> return ()


execProcess :: FilePath -> [String] -> B.ByteString -> IO (ExitCode, B.ByteString, B.ByteString)
execProcess cmd args input = do
  (inh, outh, errh, pid) <- runInteractiveProcess cmd args Nothing Nothing
  handle (\e -> do terminateProcess pid
                   throw (e::SomeException)) $ do
  outVar <- newEmptyMVar
  forkIO $ B.hGetContents outh >>= putMVar outVar
  errVar <- newEmptyMVar
  forkIO $ B.hGetContents errh >>= putMVar errVar

  when (not (B.null input)) $ do B.hPutStr inh input >> hFlush inh
  hClose inh

  out <- takeMVar outVar
  err <- takeMVar errVar
  ret <- waitForProcess pid
  return (ret, out, err)


module Config where


data Mode
    = Version
    | NumericVersion
    | SupportedLanguages
    | InstallLibrary [String]
    | Normal Config [String]

data Config
    = Config { configFiles        :: [String]
             , configGhcOptions   :: [String]
             , configBuildLibrary :: Bool
             , configNoLink       :: Bool
             , configOutputFile   :: String
             , configSrcDir       :: String
             , configDebugBuild   :: Bool
             , configProfBuild    :: Bool
             , configKeepTmpFiles :: Bool
             , configUseCc        :: String
             , configUseGrinMM    :: Bool
             , configOpt          :: OptConfig
             }

defaultConfig :: Config
defaultConfig
    = Config { configFiles        = []
             , configGhcOptions   = []
             , configBuildLibrary = False
             , configNoLink       = False
             , configOutputFile   = "a.out"
             , configSrcDir       = ""
             , configDebugBuild   = False
             , configProfBuild    = False
             , configKeepTmpFiles = False
             , configUseCc        = "gcc"
             , configUseGrinMM    = False
             , configOpt          = defaultOptConfig
             }



data OptConfig
    = OptConfig { optSmallNodeSize :: Int
                  -- ^ Size limit for small nodes. The default is 4 words. If a constructor is
                  --   bigger than this limit, it will be broken up in two blocks with the first
                  --   block containing a pointer to the next. Since we at least need room for a
                  --   tag and a pointer, the mininum value here is 2 words.
                , optStage1Iterations :: Int
                  -- ^ Default is 5.
                , optStage2Iterations :: Int
                  -- ^ Default is 5.
                , optStage3Iterations :: Int
                  -- ^ Default is 5.
                , optGCBlockSize :: Int -- GC block size in words.
                }


defaultOptConfig :: OptConfig
defaultOptConfig = OptConfig { optSmallNodeSize = 4
                             , optStage1Iterations = 5
                             , optStage2Iterations = 5
                             , optStage3Iterations = 5
                             , optGCBlockSize = 32
                             }




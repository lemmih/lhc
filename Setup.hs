import System.Cmd (system)
import System.Process (readProcess)
import System.FilePath
import Control.Monad (when, unless, liftM)
import System.Directory
import System.Info as SysVer
import Data.Version
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Simple.InstallDirs (CopyDest(..))
import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs, InstallDirs(..))

lhclibdir = "lib"
libsToBuild = map (lhclibdir </>) [ "ghc-prim", "integer-ltm", "base" ]

main = defaultMainWithHooks simpleUserHooks { postInst = myPostInst }
  where myPostInst _ _ pkgdesc buildinfo = do
          let dirs   = absoluteInstallDirs pkgdesc buildinfo NoCopyDest
              pkgVer = pkgVersion (package pkgdesc)
              lhc    = bindir dirs </> "lhc"
              lhcpkg = bindir dirs </> "lhc-pkg"
              lpkgdesc = localPkgDescr buildinfo
              exes     = executables lpkgdesc
              sanity   = any (\(Executable s _ _) -> s == "lhc") exes
          unless sanity $ fail "No lhc executale found - this probably shouldn't happen"
          let lhcexe   = head $ filter (\(Executable s _ _) -> s == "lhc") exes
              binfo    = buildInfo lhcexe
              customF  = customFieldsBI binfo
          -- initial setup
          udir' <- getAppUserDataDirectory "lhc"
          createDirectoryIfMissing True (udir' </> "packages")
          -- NOTE - THIS MUST BE KEPT IN SYNC WITH
          -- lhc-pkg in lhc-pkg/Main.hs!!!
          let udir =  udir' </> (SysVer.arch ++ "-" ++ SysVer.os ++  "-" ++ (showVersion pkgVer))
              pkgconf = udir </> "package" <.> "conf.d"
          createDirectoryIfMissing True udir
          b <- doesDirectoryExist pkgconf
          unless b $ do
            putStr "Creating initial package.conf.d database..."
            system $ "lhc-pkg init " ++ pkgconf
            putStrLn "Done"

          -- copy over extra-gcc-opts and unlit from
          -- ghc's libdir
          -- NOTE FIXME: this assumes that the 'ghc' executable
          -- points to the same one you compiled LHC against; although,
          -- the compile options would probably roughly stay the same anyway
          ghcLibdir <- liftM (unwords . words) $ readProcess "ghc" ["--print-libdir"] []
          let unlit          = ghcLibdir </> "unlit"
              extragccopts = ghcLibdir </> "extra-gcc-opts"
          putStr "Copying unlit and extra-gcc-opts..."
          system $ "cp "++unlit++" "++(udir </> "unlit")
          system $ "cp "++extragccopts++" "++(udir </> "extra-gcc-opts")
          putStrLn "Done"
          -- build libraries if -fwith-libs is passed
          when (withLibs customF) $ do
            let confargs = unwords [ "--lhc", "--with-lhc="++lhc, "--with-lhc-pkg="++lhcpkg
                                   , "--prefix="++show (prefix (installDirTemplates buildinfo))
                                   ]
            putStrLn "building libraries..."
            installLhcPkgs confargs libsToBuild

        withLibs = any $ \(x,y) -> x == "x-build-libs" && y == "True"
        installLhcPkgs cf  = mapM_ (installLhcPkg cf)
        installLhcPkg cf n = do
            putStrLn $ "\n[installing "++n++" package for lhc]\n"
            let x = unwords ["cd",n
                            ,"&&","runghc Setup configure",cf
                            ,"&&","(runghc Setup build || runghc Setup build)"
                            ,"&&","runghc Setup copy"
                            ,"&&","runghc Setup register"]
            putStrLn $ x
            system x
            putStrLn "\nDone"
            return ()

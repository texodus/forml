import Distribution.Simple

import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Simple.UserHooks
import Distribution.Package
import Distribution.PackageDescription
         ( PackageDescription(..), GenericPackageDescription
         , updatePackageDescription, hasLibs
         , HookedBuildInfo, emptyHookedBuildInfo )
import Distribution.PackageDescription.Parse
         ( readPackageDescription, readHookedBuildInfo )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.Simple.Program
         ( defaultProgramConfiguration, addKnownPrograms, builtinPrograms
         , restoreProgramConfiguration, reconfigurePrograms )
import Distribution.Simple.PreProcess (knownSuffixHandlers, PPSuffixHandler)
import Distribution.Simple.Setup
import Distribution.Simple.Command

import Distribution.Simple.Build        ( build )
import Distribution.Simple.SrcDist      ( sdist )
import Distribution.Simple.Register
         ( register, unregister )

import Distribution.Simple.Configure
         ( getPersistBuildConfig, maybeGetPersistBuildConfig
         , writePersistBuildConfig, checkPersistBuildConfigOutdated
         , configure, checkForeignDeps )

import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.BuildPaths ( srcPref)
import Distribution.Simple.Test (test)
import Distribution.Simple.Install (install)
import Distribution.Simple.Haddock (haddock, hscolour)
import Distribution.Simple.Utils
         (die, notice, info, warn, setupMessage, chattyTry,
          defaultPackageDesc, defaultHookedPackageDesc,
          rawSystemExitWithEnv, cabalVersion, topHandler )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Verbosity
import Language.Haskell.Extension
import Distribution.Version
import Distribution.License
import Distribution.Text
         ( display )

import System.Process
import System.Directory
import System.Exit
import Control.Monad   (when)
import Data.List       (intersperse, unionBy)



main = defaultMainWithHooks
           simpleUserHooks { preBuild = preHook
                           , postBuild = postHook }

alert msg = do
    putStrLn ""
    putStrLn "*************************************************************************************"
    putStrLn "****"
    putStrLn $ "**** " ++ msg
    putStrLn ""


preHook _ _ = do
    exists <- doesFileExist "prelude.obj"
    if exists then removeFile "prelude.obj" else return ()
    system "touch prelude.obj"
    alert "Building prelude-less compiler"
    return emptyHookedBuildInfo

postHook _ flags pkg_descr localbuildinfo = do
    --build pkg_descr localbuildinfo flags (allSuffixHandlers hooks)
    alert "Building prelude"
    retCode <- rawSystem "dist/build/Forml/forml" ["-no-test", "-no-prelude", "src/forml/prelude.forml"]
    case retCode of
        ExitFailure msg -> do
            alert "FAILED"
            putStrLn (show msg)
        ExitSuccess -> do
            alert "Building compiler"
            build pkg_descr localbuildinfo flags allSuffixHandlers 

allSuffixHandlers :: [PPSuffixHandler]
allSuffixHandlers =
    knownSuffixHandlers
    where
      overridesPP :: [PPSuffixHandler] -> [PPSuffixHandler] -> [PPSuffixHandler]
      overridesPP = unionBy (\x y -> fst x == fst y)
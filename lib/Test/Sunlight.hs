{-# LANGUAGE OverloadedStrings #-}
module Test.Sunlight
  ( Description
  , Compiler
  , GhcPkg
  , Cabal
  , TestInputs(..)
  , runTests
  ) where

import Distribution.Package
import Distribution.Text
import Test.Sunlight.Shell
import System.Directory
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Version
import Data.Tuple.Select
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid
import Data.Time
import System.Locale (defaultTimeLocale)
import Data.List (intersperse)
import System.Random
import Control.Monad
import System.Exit


-- | Result from installing the package's dependencies.
data InstallResult = InstallResult
  { drOutput :: CmdResult
  , drGhcPkg :: CmdResult
  } deriving Show

instance CheckOk InstallResult where
  isOk r = all isOk . map ($ r) $ [drOutput, drGhcPkg]

-- | Install a package's dependencies.

-- Preconditions:
--
-- * current directory has the unpacked tarball.
--
-- Side effects:
--
-- * dependencies are fully or partially installed.  If partially
-- installed, the ExitCode should be non-zero.
installDeps
  :: [PackageIdentifier]
  -- ^ Optional constraints.  Currently constraints may only be tied
  -- to specific versions (for instance, flag constraints or
  -- constraints tied to a range of versions are not allowed.)
  -> FilePath
  -- ^ Install using this compiler (full path to compiler).
  -> FilePath
  -- ^ Full path to ghc-pkg.
  -> FilePath
  -- ^ Path to cabal executable
  -> FilePath
  -- ^ Path to directory to use for user package DB.
  -> FilePath
  -- ^ Directory to use for installation prefix
  -> IO InstallResult
installDeps cs ghc pkg cabal db dir = do
  let opts =
        [ "install"
        , "--verbose=2"
        , "--with-compiler=" ++ ghc
        , "--with-hc-pkg=" ++ pkg
        , "--prefix=" ++ dir
        , "--disable-library-profiling"
        , "--disable-executable-profiling"
        , "--package-db=clear"
        , "--package-db=global"
        , "--package-db=" ++ db
        , "--enable-tests"
        , "--disable-documentation"
        , "--only-dependencies"
        ] ++ map constraint cs
  out <- tee cabal opts
  let pkgOpts = [ "list", "--global", "--package-conf=" ++ db ]
  pkgOut <- tee pkg pkgOpts
  return $ InstallResult out pkgOut

-- | Makes a PackageIdentifier into a version constraint option.
constraint :: PackageIdentifier -> String
constraint i = "--constraint=" ++ name ++ "==" ++ ver
  where
    name = display . pkgName $ i
    ver = display . pkgVersion $ i

data PackageInstResult = PackageInstResult
  { piSetup :: CmdResult
  , piConfigure :: CmdResult
  , piBuild :: CmdResult
  , piInst :: CmdResult
  , piGhcPkg :: CmdResult
  } deriving Show

instance CheckOk PackageInstResult where
  isOk r = all isOk . map ($ r) $
    [ piSetup, piConfigure, piBuild, piInst, piGhcPkg ]

-- | Install a package.
--
-- Preconditions:
--
-- * dependencies have already been installed at the given prefix
-- and registered in the given db
--
-- * current directory has the unpacked tarball.
--
-- Side effects:
--
-- * package should be installed, or exit code is non-zero
installPackage
  :: FilePath
  -- ^ Install using this compiler (full path to compiler)
  -> FilePath
  -- ^ Full path to ghc-pkg
  -> FilePath
  -- ^ User DB
  -> FilePath
  -- ^ Installation prefix
  -> IO PackageInstResult
installPackage ghc pkg db pfx = do
  let opts =
        [ "configure"
        , "--verbose=2"
        , "--with-compiler=" ++ ghc
        , "--with-hc-pkg=" ++ pkg
        , "--prefix=" ++ pfx
        , "--package-db=clear"
        , "--package-db=global"
        , "--package-db=" ++ db
        , "--enable-tests"
        ]
  rBuildSetup <- tee "ghc" ["--make", "Setup.hs"]
  rConf <- tee "./Setup" opts
  let bOpts =
        [ "build", "--verbose=2" ]
  rBuild <- tee "./Setup" bOpts
  rInst <- tee "./Setup" ["install", "--verbose=2"]
  rPkg <- tee pkg
    [ "list"
    , "--global"
    , "--package-conf=" ++ db
    ]
  return $ PackageInstResult rBuildSetup rConf rBuild rInst rPkg

-- | Test a package.
--
-- Preconditions:
--
-- * Package has already been built.
testPackage
  :: [(String, [String])]
  -- ^ From the package root directory, these are the commands and
  -- arguments to run to test the package.
  -> IO [CmdResult]
testPackage = mapM (uncurry tee)

data InstallAndTestResult = InstallAndTestResult
  { itDate :: UTCTime
  -- ^ Current time
  , itGhcVersion :: CmdResult
  -- ^ Result of ghc --version
  , itPkgVersion :: CmdResult
  -- ^ Result of ghc-pkg --version
  , itCabalVersion :: CmdResult
  -- ^ Result of cabal --version
  , itCompileSetup :: CmdResult
  -- ^ Result from compiling Setup.hs
  , itSdistDeps :: CmdResult
  -- ^ Result from running sdist to create tree from which to
  -- install dependencies
  , itSdistPkg :: CmdResult
  -- ^ Result from running sdist to create tree from which to
  -- install package
  , itInit :: CmdResult
  -- ^ Result from initializing user package DB
  , itDeps :: InstallResult
  , itPackage :: PackageInstResult
  , itTest :: [CmdResult]
  } deriving Show

instance CheckOk InstallAndTestResult where
  isOk r = and [ rsltsOk, testsOk,
    isOk (itDeps r), isOk (itPackage r) ]
    where
      rsltsOk = all isOk . map ($ r) $
        [ itGhcVersion, itPkgVersion, itCabalVersion, itCompileSetup,
          itSdistDeps, itSdistPkg, itInit ]
      testsOk = all isOk . itTest $ r

-- | Performs test installation.
--
-- Preconditions:
--
-- * is run from root package directory.  A temporary directory is
-- created inside this directory.  All work is done within the
-- temporary directory.
--
-- Postconditions:
--
-- * cleans up after itself; no temporary files should remain.
installAndTest
  :: [PackageIdentifier]
  -- ^ Constraints
  -> UTCTime
  -> FilePath
  -- ^ Path to compiler
  -> FilePath
  -- ^ Path to ghc-pkg
  -> FilePath
  -- ^ Path to cabal executable
  -> [(String, [String])]
  -- ^ How to test the package
  -> IO InstallAndTestResult
installAndTest cs date ghc pkg cabal test =
  withTempDirectory verbose "." "sunlight" $ \relDir -> do
    dir <- canonicalizePath relDir
    let setup = dir ++ "/Setup"
        distDeps = dir ++ "/distDeps"
        distPkg = dir ++ "/distPkg"
        db = dir ++ "/db"
        pfx = dir ++ "/prefix"
    ghcVer <- tee ghc ["--version"]
    pkgVer <- tee pkg ["--version"]
    cblVer <- tee "cabal" ["--version"]
    rSetup <- tee ghc ["--make", "-outputdir", dir,
      "-o", setup, "Setup.hs"]
    rDistDeps <- tee setup
      ["sdist", "--output-directory=" ++ distDeps ]
    rDistPkg <- tee setup
      ["sdist", "--output-directory=" ++ distPkg ]
    createDirectory pfx
    rInit <- tee pkg ["init", db]
    rDeps <- inDirectory distDeps $
      installDeps cs ghc pkg cabal db pfx
    rInst <- inDirectory distPkg
      $ installPackage ghc pkg db pfx
    rTest <- inDirectory distPkg $ testPackage test
    return $ InstallAndTestResult date ghcVer pkgVer cblVer
      rSetup rDistDeps rDistPkg
      rInit rDeps rInst rTest

-- | Gets a list of PackageIdentifier with the lowest possible
-- versions.  Fails if a package has a dependency range with no
-- minimum.
lowestVersions
  :: GenericPackageDescription
  -> Either Dependency [PackageIdentifier]
  -- ^ Left with the bad dependency if there is one; Right
  -- otherwise.
lowestVersions pd = mapM lowestVersion ls
  where
    ls = depsLib ++ depsExe ++ depsTest ++ depsBench
    depsLib = case condLibrary pd of
      Nothing -> []
      Just deps -> getDependencies deps
    getDeps = getDepsList pd
    depsExe = getDeps condExecutables
    depsTest = getDeps condTestSuites
    depsBench = getDeps condBenchmarks

getDepsList
  :: GenericPackageDescription
  -> (GenericPackageDescription -> [(a, CondTree b [Dependency] c)])
  -> [Dependency]
getDepsList d f = concatMap getDependencies . map snd . f $ d

getDependencies
  :: CondTree v [Dependency] a
  -> [Dependency]
getDependencies t =
  let this = condTreeConstraints t
      rest = concatMap getDependencies . map sel2
        . condTreeComponents $ t
  in this ++ rest

lowestVersion :: Dependency -> Either Dependency PackageIdentifier
lowestVersion d@(Dependency n r) = case asVersionIntervals r of
  [] -> Left d
  (LowerBound v b, _):_
    | b == ExclusiveBound -> Left d
    | otherwise -> Right $ PackageIdentifier n v

testLowestVersions
  :: UTCTime
  -> FilePath
  -- ^ Path to compiler
  -> FilePath
  -- ^ Path to ghc-pkg
  -> FilePath
  -- ^ Path to cabal executable
  -> [(String, [String])]
  -- ^ How to test the package
  -> GenericPackageDescription
  -> Either Dependency (IO InstallAndTestResult)
testLowestVersions date ghc pkg cabal test d = case lowestVersions d of
  Left e -> Left e
  Right ps -> Right $ installAndTest ps date ghc pkg cabal test

testDefaultVersions
  :: UTCTime
  -> FilePath
  -- ^ Path to compiler
  -> FilePath
  -- ^ Path to ghc-pkg
  -> FilePath
  -- ^ Path to cabal executable
  -> [(String, [String])]
  -- ^ How to test the package
  -> IO InstallAndTestResult
testDefaultVersions date ghc pkg cabal test =
  installAndTest [] date ghc pkg cabal test


testMultipleVersions
  :: UTCTime
  -> (FilePath, FilePath)
  -- ^ Compiler and ghc-pkg to use when testing lowest version
  -> [(FilePath, FilePath)]
  -- ^ Compilers and ghc-pkg to use to test default versions
  -> FilePath
  -- ^ Path to cabal executable
  -> [(String, [String])]
  -- ^ How to test package
  -> GenericPackageDescription
  -> Either Dependency
            (IO (InstallAndTestResult, [InstallAndTestResult]))
testMultipleVersions date (lowestGhc, lowestPkg) rest cabal test pd =
  case testLowestVersions date lowestGhc lowestPkg cabal test pd of
    Left d -> Left d
    Right g -> Right $ do
      r1 <- g
      let testRest (c, p) = testDefaultVersions date c p cabal test
      rs <- mapM testRest rest
      return (r1, rs)

-- Sample test directory structure:
-- minimum-versions.txt
-- current-versions.txt
-- sunlight
--  - 2014
--    - 02
--      - 22
--        - UTCtime
--          - [random string]
--            - lowest-7.4.2-passed.txt
--            - current-7.4.2-passed.txt
--            - current-7.6.1-passed.txt

versionsReport
  :: BS8.ByteString
  -- ^ Description of dependencies
  -> Compiler
  -> Description
  -> UTCTime
  -> InstallAndTestResult
  -> BS8.ByteString
versionsReport desc c d t r = BS8.concat
  [ "This package was tested to work with these dependency\n"
  , "versions and compiler version.\n"
  , desc
  , "Tested as of: " <> (BS8.pack . show $ t) <> "\n"
  , "Path to compiler: " <> BS8.pack c <> "\n"
  , "Compiler description: " <> BS8.pack d <> "\n"
  , "\n"
  ] <>
  (crStdOut . piGhcPkg . itPackage $ r)

minimumVersionsReport
  :: Compiler
  -> Description
  -> UTCTime
  -> InstallAndTestResult
  -> BS8.ByteString
minimumVersionsReport = versionsReport
  "These are the minimum versions given in the .cabal file.\n"

writeMinimumVersionsReport :: BS8.ByteString -> IO ()
writeMinimumVersionsReport = BS8.writeFile "minimum-versions.txt"

currentVersionsReport
  :: UTCTime
  -> [(Description, Compiler, a)]
  -> [InstallAndTestResult]
  -> Maybe BS8.ByteString
currentVersionsReport t ds ts
  | null ds || null ts = Nothing
  | otherwise = Just $ versionsReport dep comp desc t r
  where
    dep = "These are the default versions fetched by cabal install.\n"
    (desc, comp, _) = last ds
    r = last ts

writeCurrentVersionsReport :: BS8.ByteString -> IO ()
writeCurrentVersionsReport = BS8.writeFile "current-versions.txt"


instance ShowBS InstallResult where
  showBS r = showBS (drOutput r)
    <> showBS (drGhcPkg r)

instance ShowBS PackageInstResult where
  showBS r =
    showBS (piSetup r)
    <> showBS (piConfigure r)
    <> showBS (piBuild r)
    <> showBS (piInst r)
    <> showBS (piGhcPkg r)

instance ShowBS InstallAndTestResult where
  showBS i =
    "Current time: " <> showBS (itDate i) <> "\n"
    <> showBS (itGhcVersion i)
    <> showBS (itPkgVersion i)
    <> showBS (itCabalVersion i)
    <> showBS (itCompileSetup i)
    <> showBS (itSdistDeps i)
    <> showBS (itSdistPkg i)
    <> showBS (itInit i)
    <> showBS (itDeps i)
    <> showBS (itPackage i)
    <> (BS8.concat . map showBS . itTest $ i)

-- | Gets four-character random string.
randomString :: IO String
randomString =
  fmap (map toEnum)
  $ replicateM 4 (getStdRandom (randomR (fromEnum 'a', fromEnum 'z')))

makeResultFile
  :: UTCTime
  -> String
  -- ^ Random string
  -> String
  -- ^ Description string, e.g. "lowest" or "current"
  -> String
  -- ^ Compiler version description
  -> InstallAndTestResult
  -> IO ()
makeResultFile utct rand desc comp res = do
  let dir = resultDirectory utct rand
  createDirectoryIfMissing True dir
  let fn = dir ++ "/" ++ desc ++ "-" ++ comp ++ "-"
        ++ passFail ++ ".txt"
      passFail
        | isOk res = "passed"
        | otherwise = "FAILED"
  BS8.writeFile fn . showBS $ res


resultDirectory
  :: UTCTime
  -> String
  -- ^ Random string
  -> String
resultDirectory t s = concat . intersperse "/" $
  ["sunlight", yr, mo, dy, ti, s ]
  where
    (y, m, d) = toGregorian . utctDay $ t
    yr = show y
    mo = pad . show $ m
    dy = pad . show $ d
    pad str
      | length str > 1 = str
      | otherwise = '0':str
    ti = formatTime defaultTimeLocale "%H%M%S" t

-- | A description for this compiler version.  This will be used in
-- the directory name in the tree that is written to disk.  I simply
-- use a compiler version, such as @7.4@.
type Description = String

-- | Path to GHC compiler.  You can use a full path name or, if you use
-- just an executable name, the PATH will be searched.
type Compiler = String

-- | Path to ghc-pkg.  If you use just an executable name, the PATH
-- will be searched.
type GhcPkg = String

-- | Path to cabal executable.  Used to install package
-- dependencies.  If you use just an executable name, the PATH will
-- be searched.
type Cabal = String

data TestInputs = TestInputs
  { tiDescription :: Maybe GenericPackageDescription
  -- ^ If Just, use this package description.  Handy if you write
  -- your own package description in Haskell rather than in the
  -- cabal format.  Otherwise, use Nothing and sunlight will look
  -- for the default cabal file and parse that.

  , tiCabal :: Cabal
  -- ^ Which @cabal@ executable to use.

  , tiLowest :: (Description, Compiler, GhcPkg)
  -- ^ Test the minimum dependency bounds using this compiler.  A
  -- report is left in the main package directory showing which
  -- package versions worked with this compiler and with the minimum
  -- bounds.

  , tiDefault :: [(Description, Compiler, GhcPkg)]
  -- ^ Test the default dependencies using these compilers.  Since
  -- cabal-install will eagerly get the most recent dependencies it
  -- can find, this will test the highest possible versions.  The
  -- compiler specified in 'tiLowest' is not automatically retried
  -- here, so if you want to use that compiler specify it as well.
  --
  -- The last compiler in this list is assumed to be the most recent
  -- compiler.  A report is left in the main package directory
  -- showing the dependencies that worked with this compiler
  -- version.

  , tiTest :: [(String, [String])]
  --  ^ How to test the package.  Each pair is a command to run,
  --  and the arguments to that command.  The command is run from
  --  the resulting package root directory after the package is
  --  unpacked and compiled.  So, if your test suite is an
  --  executable called @myTest@ and it requires the argument
  --  @--verbose@, you would use something like this:
  --
  -- > [("dist/build/myTest/myTest", ["--verbose"])]
  --
  -- The tests are considered to pass if all these commands exit
  -- with a zero exit status.
  --
  -- You can also abuse this option to get output in the record of
  -- the test; for instance, maybe you want to record the current
  -- git HEAD:
  --
  -- > [("git", ["rev-parse", "HEAD"])]
  } deriving Show

runTests :: TestInputs -> IO ()
runTests i = do
  desc <- case tiDescription i of
    Just pd -> return pd
    Nothing -> do
      path <- defaultPackageDesc verbose
      readPackageDescription verbose path
  date <- getCurrentTime
  randStr <- randomString
  let last2 (_, b, c) = (b, c)
      eiRes = testMultipleVersions date
        (last2 . tiLowest $ i) (map last2 . tiDefault $ i)
        (tiCabal i) (tiTest i) desc
  (r1, rs) <- case eiRes of
    Left e -> dependencyError e
    Right g -> g
  makeResultFile date randStr "lowest" (sel1 . tiLowest $ i) r1
  let makeRest r (d, _, _) = makeResultFile date randStr "current" d r
  _ <- zipWithM makeRest rs (tiDefault i)
  when (not $ isOk r1 && all isOk rs) exitFailure
  writeMinimumVersionsReport $ minimumVersionsReport
    (sel2 . tiLowest $ i) (sel1 . tiLowest $ i) date r1
  maybe (return ()) writeCurrentVersionsReport $ currentVersionsReport
    date (tiDefault i) rs
  exitSuccess

dependencyError :: Dependency -> IO a
dependencyError d = putStrLn s >> exitFailure
  where
    s = "dependency invalid: " ++ show d ++ " sunlight requires "
      ++ "that you specify a fixed lower bound for each dependency."

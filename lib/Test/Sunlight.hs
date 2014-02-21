module Test.Sunlight where

import Control.Exception
import System.Directory
import Distribution.Version
import System.Exit
import Distribution.Package

-- | Runs an IO action within the given directory, and then returns
-- to the original directory.
inDirectory
  :: FilePath
  -> IO a
  -> IO a
inDirectory inside a = bracket get rel run
  where
    get = do
      d <- getCurrentDirectory
      setCurrentDirectory inside
      return d
    rel = setCurrentDirectory
    run = const a

-- | Result from installing the package's dependencies.
data InstallDepsResult = InstallDepsResult
  { drStdOut :: String
    -- ^ Standard output from build process
  , drStdErr :: String
    -- ^ Standard error from build process
  , drExitCode :: ExitCode
    -- ^ Exit code
  , drGhcPkg :: String
    -- ^ Output from ghc-pkg showing which packages where actually
    -- installed.
  } deriving Show

-- | Install a package's dependencies.

-- Preconditions:
--
-- * current directory has the unpacked tarball.
--
-- Side effects:
--
-- * dependencies are fully or partially installed.  If partially
-- installed, the ExitCode should be non-zero.
--
-- * does not remove the installed dependencies.
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
  -- ^ Path to directory to use for user package DB.
  -> IO InstallDepsResult
installDeps = undefined

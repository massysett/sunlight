module Test.Sunlight where

import qualified Data.ByteString as BS
import Control.Exception
import Foreign.Safe
import System.Directory
import Distribution.Version
import System.Exit
import Distribution.Package
import System.IO
import System.Process
import Control.Concurrent.MVar

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

-- | Creates a process.  Its stdin is closed.  Returns handles to
-- stdout and stderr, and the ProcessHandle.
makeProcess
  :: FilePath
  -- ^ Run this command
  -> [String]
  -- ^ Arguments
  -> IO (Handle, Handle, ProcessHandle)
makeProcess c as = do
  (Just hin, Just out, Just err, h) <- createProcess cp
  hClose hin
  return (out, err, h)
  where
    cp = CreateProcess { cmdspec = RawCommand c as
                       , cwd = Nothing
                       , env = Nothing
                       , std_in = CreatePipe
                       , std_out = CreatePipe
                       , std_err = CreatePipe
                       , close_fds = True
                       , create_group = False }

bufsize :: Int
bufsize = 512

-- | Runs a command, sending standard output and standard error to
-- the terminal, while also returning the standard output.
tee
  :: FilePath
  -- ^ Run this command
  -> [String]
  -- ^ Arguments
  -> IO (ExitCode, BS.ByteString, BS.ByteString)
tee cmd as = bracket (makeProcess cmd as) rel go
  where
    rel (out, err, _) = hClose out >> hClose err

    go (out, err, hdl) = do
      mvOut <- newEmptyMVar
      mvErr <- newEmptyMVar
      undefined

-- | Reads from standard 

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

{-# LANGUAGE OverloadedStrings #-}
module Test.Sunlight.Shell where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Exception
import Data.Monoid
import Foreign.Safe
import System.Directory
import System.Exit
import System.IO
import System.Process
import Control.Concurrent
import Data.Time

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
      putStrLn $ "current directory is: " ++ d
      putStrLn $ "changing to directory: " ++ inside
      setCurrentDirectory inside
      return d
    rel d = do
      putStrLn $ "returning to directory: " ++ d
      setCurrentDirectory d
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
  d <- getCurrentDirectory
  putStrLn $ "current directory is: " ++ d
  putStrLn $ "running: " ++ c ++ " " ++ show as
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

data CmdResult = CmdResult
  { crCode :: ExitCode
  , crStdOut :: BS.ByteString
  , crStdErr :: BS.ByteString
  , crCommand :: String
  , crArgs :: [String]
  } deriving Show

class ShowBS a where
  showBS :: a -> BS.ByteString

instance ShowBS CmdResult where
  showBS r =
    BS8.pack (crCommand r) <> " " <> (BS8.pack . show . crArgs $ r)
    <> " "
    <> case crCode r of
        ExitSuccess -> "succeeded with exit code 0"
        ExitFailure e -> "FAILED with exit code " <> BS8.pack (show e)
    <> "\n"
    <> "Standard output:\n"
    <> crStdOut r
    <> "\nStandard error:\n"
    <> crStdErr r
    <> "\n"

instance ShowBS UTCTime where
  showBS = BS8.pack . show


resultOk :: CmdResult -> Bool
resultOk c = case crCode c of
  ExitSuccess -> True
  _ -> False

class CheckOk a where
  isOk :: a -> Bool

instance CheckOk CmdResult where
  isOk c = case crCode c of
    ExitSuccess -> True
    _ -> False

-- | Runs a command, sending standard output and standard error to
-- the terminal, while also returning the standard output.
tee
  :: FilePath
  -- ^ Run this command
  -> [String]
  -- ^ Arguments
  -> IO CmdResult
tee cmd as = bracket (makeProcess cmd as) rel go
  where
    rel (out, err, _) = hClose out >> hClose err

    go (out, err, hdl) = do
      mvOut <- newEmptyMVar
      mvErr <- newEmptyMVar
      _ <- forkIO (readToEOF out stdout >>= putMVar mvOut)
      _ <- forkIO (readToEOF err stderr >>= putMVar mvErr)
      bsOut <- takeMVar mvOut
      bsErr <- takeMVar mvErr
      cd <- waitForProcess hdl
      return $ CmdResult cd bsOut bsErr cmd as


-- | Reads from the given handle until EOF.  Sends results to the
-- given handle and also stores them in the returned ByteString.
readToEOF
  :: Handle
  -- ^ Read from this handle
  -> Handle
  -- ^ Send results to this handle
  -> IO BS.ByteString
readToEOF inp out = allocaBytes bufsize $ \ptr ->
  let go bs = do
        n <- hGetBufSome inp ptr bufsize
        case n of
          0 -> return bs
          _ -> do
            hPutBuf out ptr n
            bs' <- BS.packCStringLen (ptr, n)
            go (bs `BS.append` bs')
  in go BS.empty


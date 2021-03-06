{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Data.ExternalSort.Internal
where


import           Control.Exception
import           Control.Monad
import           Data.IORef
import           Pipes
import qualified Pipes.Prelude as P
import           Pipes.Interleave
import           System.Directory (removeFile)
import           System.IO
import           System.Posix.Temp

-- friends
-- import           Data.ExternalSort.VectorSort (vectorSort)

-- TODO
-- 0. Use handles instead of file paths in the pipes. Makes closing easier.
-- 1. Clean up on Ctrl-C
-- 2. Put temp files /tmp
-- 3. Make concurrent using Pipes.Concurrent
-- 4. Try unchunking. Check performance


data ExternalSortCfg a =
  ExternalSortCfg {
      -- | @readVal h@ is responsible for reading one value from a handle
      readVal  :: Handle -> IO a
      -- | @writeVal h r@ writes a single value to a handle
    , writeVal :: Handle -> a -> IO ()
      -- | the number of values in each chunk
    , chunkSize :: Int
      -- | a function that sorts each chunk
    , sorter :: [a] -> [a]
      -- | a function that compares two values
    , comparer :: a -> a -> Ordering
    }



externalSortFile :: ExternalSortCfg a -> FilePath -> FilePath -> IO ()
externalSortFile cfg inFile outFile = do
  withFile inFile ReadMode   $ \inH  ->
    withFile outFile WriteMode $ \outH ->
      externalSortHandle cfg inH outH

externalSortHandle :: ExternalSortCfg a -> Handle -> Handle -> IO ()
externalSortHandle cfg inH outH = do
  -- fileRef is used to clean up intermediate files when exception occurs
  fileRef <- newIORef []
  finally
    (do sortAndWriteToChunks cfg fileRef inH
        runEffect $ fileMerger cfg fileRef outH)
    (do files <- readIORef fileRef
        mapM_ removeFile files)


-- a pipe that reads in a chunk of a file and sorts it
chunkSorter :: (Monad m) => ExternalSortCfg a -> Pipe [a] [a] m ()
chunkSorter = P.map . sorter

chunkReader :: Int -> (Handle -> IO a) -> Handle -> Producer' [a] IO ()
chunkReader chunkSz readF inH = go
  where
    go = do
      etAs <- lift $ readUpToN chunkSz readF inH
      case etAs of
        Left as  -> yield as
        Right as -> yield as >> go

--
-- @Left as@ means that EOF has been reached as @as@ returned. May not be @n@ long.
-- @Right as@ means that EOF has not yet been reached as @n@ @as@ have been read.
--
{-# INLINE readUpToN #-}
readUpToN :: Int -> (Handle -> IO a) -> Handle -> IO (Either [a] [a])
readUpToN n rd h = go 0 []
  where
    go i as
      | i < n = do
        eof <- hIsEOF h
        if eof
         then do
          return $ Left $ reverse as
         else do
          a <- rd h
          go (i+1) (a:as)
      | otherwise = return $ Right $ reverse as

chunkWriter :: ExternalSortCfg a -> IORef [FilePath] -> Consumer [a] IO ()
chunkWriter cfg fileRef = go
  where
    go = do
      as <- await -- [as] is sorted
      lift $ do
        (file,h) <- mkstemp "sort-chunk."
        modifyIORef fileRef (file:)
        finally
          (mapM_ (writeVal cfg h) as)
          (hClose h)
      go

--
-- fromHandle' is like fromHandle except that it uses a function @f@ to read from the Handle rather
-- than reading a 'String'
--
{-# INLINE fromHandle' #-}
fromHandle' :: MonadIO m => (Handle -> IO a) -> Handle -> Producer' a m ()
fromHandle' f h = go
  where
    go = do
      eof <- liftIO $ hIsEOF h
      when (not eof) $ do
        r <- liftIO (f h)
        yield r
        go

singleReader :: (Handle -> IO a) -> FilePath -> Producer' a IO ()
singleReader reader inFile = do
  h  <- lift $ openFile inFile ReadMode
  fromHandle' reader h

--
-- @sortAndWriteToChunks@ reads a number of chunks from the input handle, sorts them, and
-- writes them out to intermediate files containing @chunkSize cfg@ values.
-- The paths of the intermediate files are stored in @fileRef@.
-- (This is so if there is an exception and the program needs to abort we can clean these files
-- up.)
--
sortAndWriteToChunks :: forall a. ExternalSortCfg a -> IORef [FilePath]
                     -> Handle -> IO ()
sortAndWriteToChunks cfg fileRef inH = runEffect producer
  where
    producer :: Effect IO ()
    producer = chunkReader (chunkSize cfg) (readVal cfg) inH >->
               chunkSorter cfg >-> chunkWriter cfg fileRef
------------

fileMerger :: forall a. ExternalSortCfg a -> IORef [FilePath] -> Handle -> Effect IO ()
fileMerger cfg fileRef outH = do
  files <- lift $ readIORef fileRef
  producer files >-> consumer outH
  where
    producer :: [FilePath] -> Producer a IO ()
    producer files = interleave (comparer cfg) kReaders
      where
        kReaders :: [Producer a IO ()]
        kReaders = map (singleReader (readVal cfg)) files
    consumer h = P.mapM_ (writeVal cfg h)
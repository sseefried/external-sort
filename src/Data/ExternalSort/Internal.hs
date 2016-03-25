{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Data.ExternalSort.Internal
where


import           Control.Exception
import           Control.Monad
import           Data.List
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
      -- @readRec h@ is responsible for reading one record from a handle
      mscReadRec  :: Handle -> IO a
      -- @mscWriteRec h r@ writes a single record to a handle
    , mscWriteRec :: Handle -> a -> IO ()
    , mscChunkSize :: Int -- the number of records read per chunk
    }



externalSortFile :: Ord a => ExternalSortCfg a -> FilePath -> FilePath -> IO ()
externalSortFile cfg inFile outFile = do
  -- TODO: Use bracket here
  withFile inFile ReadMode   $ \inH  ->
    withFile outFile WriteMode $ \outH ->
      externalSortHandle cfg inH outH

externalSortHandle :: Ord a => ExternalSortCfg a -> Handle -> Handle -> IO ()
externalSortHandle cfg inH outH = do
  fileRef <- newIORef []
  finally
    (do files <- sortAndWriteToChunks fileRef cfg inH
        runEffect $ fileMerger cfg files outH)
    (do files <- readIORef fileRef
        mapM_ removeFile files)


-- a pipe that reads in a chunk of a file and sorts it
chunkSorter :: (Ord a, Monad m)  => Pipe [a] [a] m ()
chunkSorter = P.map sort -- TODO: use vector sort

chunkReader :: Int -> (Handle -> IO a) -> Handle -> Producer' [a] IO ()
chunkReader chunkSize readF inH = go
  where
    go = do
      etAs <- lift $ readUpToN chunkSize readF inH
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

chunkWriter :: IORef [FilePath] -> ExternalSortCfg a -> Pipe [a] FilePath IO ()
chunkWriter fileRef cfg = go
  where
    go = do
      as <- await -- [as] is sorted
      file <- lift $ do
        (file,h) <- mkstemp "sort-chunk."
        modifyIORef fileRef (file:)
        finally
          (mapM_ (mscWriteRec cfg h) as)
          (hClose h)
        return file
      yield file
      go


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
singleReader readRec inFile = do
  h  <- lift $ openFile inFile ReadMode
  fromHandle' readRec h

--
-- takes an input file, reads it, writes out to n temporary sorted files
--
sortAndWriteToChunks :: forall a. (Ord a) => IORef [FilePath] -> ExternalSortCfg a -> Handle
                     -> IO [FilePath]
sortAndWriteToChunks fileRef cfg inH = reverse <$> P.toListM producer
  where
    producer :: Producer FilePath IO ()
    producer = chunkReader (mscChunkSize cfg) (mscReadRec cfg) inH >->
               chunkSorter >-> chunkWriter fileRef cfg
------------

fileMerger :: forall a. Ord a => ExternalSortCfg a -> [FilePath] -> Handle -> Effect IO ()
fileMerger cfg files outH = do
  producer >-> consumer outH
  where
    kReaders :: [Producer a IO ()]
    kReaders = map (\f -> singleReader (mscReadRec cfg) f) files
    producer :: Producer a IO ()
    producer = interleave compare kReaders
    consumer h = P.mapM_ (mscWriteRec cfg h)

squeeze :: Monad m => Pipe [a] a m r
squeeze = P.concat -- special case of Foldable a => Pipe (f a) a m r

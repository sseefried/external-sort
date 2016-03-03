{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Data.ExternalMergeSort.Internal
where

import           Data.List
import           Pipes
import qualified Pipes.Prelude as P
import           Pipes.Interleave
import           System.Directory (removeFile)
import           System.IO
import           System.Posix.Temp

-- friends
-- import           Data.ExternalMergeSort.VectorSort (vectorSort)

-- TODO
-- 0. Use handles instead of file paths in the pipes. Makes closing easier.
-- 1. Clean up on Ctrl-C
-- 2. Put temp files /tmp
-- 3. Make concurrent using Pipes.Concurrent
-- 4. Try unchunking. Check performance


data MergeSortCfg a =
  MergeSortCfg {
      -- @readRec h@ is responsible for reading one record from a handle
      mscReadRec  :: Handle -> IO a
      -- @mscWriteRec h r@ writes a single record to a handle
    , mscWriteRec :: Handle -> a -> IO ()
    , mscChunkSize :: Int -- the number of records read per chunk
    }

externalMergeSort :: Ord a => MergeSortCfg a -> FilePath -> FilePath -> IO ()
externalMergeSort cfg inFile outFile = do
  -- TODO: Use bracket here
  files <- sortAndWriteToChunks cfg inFile
  runEffect $ fileMerger cfg files outFile
  mapM_ removeFile files

-- a pipe that reads in a chunk of a file and sorts it
chunkSorter :: (Ord a, Monad m)  => Pipe [a] [a] m ()
chunkSorter = P.map sort -- TODO: use vector sort

reader :: Int -> (Handle -> IO a) -> FilePath -> Producer' [a] IO ()
reader chunkSize readF inFile = do
  h  <- lift $ openFile inFile ReadMode
  run h
  where
    run h = do
      etAs <- lift $ readUpToN chunkSize readF h
      case etAs of
        Left as  -> yield as
        Right as -> yield as >> run h

--
-- @Left as@ means that EOF has been reached as @as@ returned. May not be @n@ long.
-- @Right as@ means that EOF has not yet been reached as @n@ @as@ have been read.
--
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


writer :: MergeSortCfg a -> String -> Pipe [a] FilePath IO ()
writer cfg prefix = go
  where
    go = do
      as <- await -- [as] is sorted
      -- TODO: use one lift and bracket within.
      (file, h) <- lift $ mkstemp prefix
      lift $ mapM_ (mscWriteRec cfg h) as
      lift $ hClose h
      yield file
      go

--
-- takes an input file, reads it, writes out to n temporary sorted files
--
sortAndWriteToChunks :: forall a. (Ord a) => MergeSortCfg a -> FilePath -> IO [FilePath]
sortAndWriteToChunks cfg inFile = reverse <$> P.fold (flip (:)) [] id producer -- TODO: Consider P.toList
  where
    producer :: Producer FilePath IO ()
    producer = reader (mscChunkSize cfg) (mscReadRec cfg) inFile >->
               chunkSorter >-> writer cfg inFile
------------


fileMerger :: forall a. Ord a => MergeSortCfg a -> [FilePath] -> FilePath -> Effect IO ()
fileMerger cfg files outFile = do
  h <- lift $ openFile outFile WriteMode
  producer >-> consumer h
  lift $ hClose h
  where
    numFiles = length files
    perFileChunkSize = mscChunkSize cfg `div` (numFiles + 1)
    kReaders :: [Producer a IO ()]
    kReaders = map (\f -> reader perFileChunkSize (mscReadRec cfg) f >-> squeeze) files
    producer :: Producer a IO ()
    producer = interleave compare kReaders
    consumer h = P.mapM_ (mscWriteRec cfg h)

squeeze :: Monad m => Pipe [a] a m r
squeeze = P.concat -- special case of Foldable a => Pipe (f a) a m r

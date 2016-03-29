{-# LANGUAGE ScopedTypeVariables #-}
module TestUtil where

import Data.ExternalSort.Internal

import           Control.Monad
import qualified Data.Binary          as Bin
import qualified Data.ByteString.Lazy as LB
import           Data.Int
import           Data.List
import           System.IO
import           System.Random

genRandomFile :: FilePath -> Int -> IO FilePath
genRandomFile tmpDir n = do
  (path, inH) <- openTempFile tmpDir "unsorted.txt."
  let write = do
        (i :: Int32) <- randomRIO (0,maxBound)
        writeInt32 inH i
  replicateM_ n write
  hClose inH
  return path

genRandomFileAndOpen :: FilePath -> Int -> IO (FilePath, Handle)
genRandomFileAndOpen tmpDir n = do
  path <- genRandomFile tmpDir n
  h <- openFile path ReadMode
  return (path, h)

genOutputFileName :: FilePath -> IO FilePath
genOutputFileName tmpDir = do
  (path, outH) <- openTempFile tmpDir "sorted.txt."
  hClose outH
  return path

readInt32 :: Handle -> IO Int32
readInt32 h = Bin.decode <$> LB.hGet h 4

writeInt32 :: Handle -> Int32 -> IO ()
writeInt32 h i = LB.hPut h (Bin.encode i)

int32SortCfgOfSize :: Int -> ExternalSortCfg Int32
int32SortCfgOfSize chunkSize = ExternalSortCfg readInt32 writeInt32 chunkSize sort compare

isFileSorted :: forall a. (Ord a) => ExternalSortCfg a -> FilePath -> IO Bool
isFileSorted cfg path = do
  h <- openFile path ReadMode
  recs <- readVals h
  return (recs == sort recs)
  where
    readVals :: Ord a => Handle -> IO [a]
    readVals h = go
      where
        go = do
          eof <- hIsEOF h
          if (not eof)
           then do
            rec  <- readVal cfg h
            recs <- go
            return (rec:recs)
           else do
            return []

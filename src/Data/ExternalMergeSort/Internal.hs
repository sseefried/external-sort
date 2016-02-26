{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Data.ExternalMergeSort.Internal (
  sortAndWriteToChunks,
  MergeSortCfg(..)
) where

import           Control.Monad
import           Control.Arrow
import           Data.IORef
--import           Data.Vector (Vector, (!), (//))
--import qualified Data.Vector as V
import           Pipes
import qualified Pipes.Prelude as P
import           System.Environment
import           System.IO
import           System.Posix.Temp
import           Data.Either
import           Data.Function
import           Data.List
--import           Data.Sequence (Seq, (|>) )
--import qualified Data.Sequence as S
import           Data.Maybe


data MergeSortCfg a =
  MergeSortCfg {
      -- @readRec h@ is responsible for read one record from a handle
      readRec  :: Handle -> IO a
      -- @writeRec h r@ writes a single record to a handle
    , writeRec :: Handle -> a -> IO ()
    , chunkSize :: Int -- the number of records read per chunk
    }

externalMergeSort :: Ord a => FilePath -> FilePath -> MergeSortCfg a -> IO ()
externalMergeSort inFile outFile cfg = error "externalMergeSort not defined yet"

-- a pipe that reads in a chunk of a file and sorts it
chunkSorter :: (Ord a, Monad m)  => Pipe [a] [a] m ()
chunkSorter = await >>= yield . sort >> chunkSorter

reader :: forall a. MergeSortCfg a -> FilePath -> Producer' [a] IO ()
reader cfg inFile = do
  h  <- lift $ openFile inFile ReadMode
  run h
  where
    run h = do
      etAs <- lift $ readN h
      case etAs of
        Left as  -> yield as
        Right as -> yield as >> run h
    readN :: Handle -> IO (Either [a] [a])
    readN h = go 0 []
      where
        go i as
          | i < chunkSize cfg = do
            eof <- hIsEOF h
            if eof
             then do
              return $ Left $ reverse as
             else do
              a <- readRec cfg h
              go (i+1) (a:as)
          | otherwise = return $ Right $ reverse as

writer :: MergeSortCfg a -> String -> Pipe [a] FilePath IO ()
writer cfg prefix = go
  where
    go = do
      as <- await -- [as] is sorted
      (file, h) <- lift $ mkstemp prefix
      lift $ mapM_ (writeRec cfg h) as
      yield file
      go


--
-- takes an input file, reads it, writes out to n temporary sorted files
--
sortAndWriteToChunks :: forall a. (Ord a) => MergeSortCfg a -> FilePath -> IO [FilePath]
sortAndWriteToChunks cfg inFile = reverse <$> P.fold (flip (:)) [] id producer
  where
    producer :: Producer FilePath IO ()
    producer = reader cfg inFile >-> chunkSorter >-> writer cfg inFile
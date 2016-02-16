module Data.ExternalMergeSort.Internal where

import           Control.Monad
import           Data.IORef
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Pipes
import           System.Environment
import           System.IO
import           Data.List


chunkSize :: Int
chunkSize = 2^(20::Int)

data MergeSortCfg a =
  MergeSortCfg {
      -- @readRec h@ is responsible for read one record from a handle
      readRec  :: Handle -> IO a
      -- @writeRec h r@ writes a single record to a handle
    , writeRec :: Handle -> a -> IO ()
    -- returns the size, in bytes, of a record
    , sizeRec  :: a -> Int -- make this O(1)
    }

externalMergeSort :: Ord a => FilePath -> FilePath -> MergeSortCfg a -> IO ()
externalMergeSort inFile outFile cfg = undefined

--
-- Merges the two smallest files together, then the next two smallest and so on
-- until it is complete.
--
--
mergeMultiple :: Ord a => [FilePath] -> MergeSortCfg a ->  IO FilePath
mergeMultiple fs cfg = error "mergeMultiple not implemented"

merge :: Ord a => FilePath -> FilePath -> MergeSortCfg a -> IO ()
merge inF outF cfg = error "merge not implemented"

mergeTwoP :: Ord a => Handle -> Handle -> Handle -> MergeSortCfg a -> Effect IO ()
mergeTwoP inH inH' outH cfg = mergeReaderP cfg >-> mergeRecsP >-> mergeWriterP cfg

mergeReaderP :: Ord a => MergeSortCfg a -> Producer ([a],[a]) IO ()
mergeReaderP = error "mergeReaderP not implemented"

mergeRecsP :: Ord a => Pipe ([a],[a]) [a] IO ()
mergeRecsP = do
  (xs,ys) <- await
  yield (sort (xs ++ ys))

mergeWriterP :: Ord a => MergeSortCfg a -> Consumer [a] IO ()
mergeWriterP = error "mergeWriterP not implemented"


----------------------------------------------------------------------------------------------------

--reader :: Handle -> (Handle -> IO [a]) -> Producer [a] IO ()
--reader h getChunk = go
--  where
--    go = do
--      chunk <- lift (getChunk h)
--      when (not . null $ chunk) $ do
--        yield chunk
--        go

--writer :: Handle -> (Handle -> [a] -> IO ()) -> Consumer [a] IO ()
--writer h writeChunk = go
--  where
--    go = do
--      chunk <- await
--      lift (writeChunk h chunk)
--      go

--readNLines :: Int -> Handle -> IO [ByteString]
--readNLines n h = go 0
--  where
--    go i
--     | i >= n    = return []
--     | otherwise = do
--         eof <- hIsEOF h
--         if not eof then do
--           line  <- B.hGetLine h
--           lines <- go (i+1)
--           return (line:lines)
--         else return []

--writeLines :: Handle -> [ByteString] -> IO ()
--writeLines h lines = mapM_ (B.hPutStrLn h) lines

--pipe :: FilePath -> FilePath -> Effect IO ()
--pipe inFile outFile = do
--  inH <- lift $ openFile inFile ReadMode
--  outH <- lift $ openFile outFile WriteMode
--  reader inH (readNLines chunkSize) >-> writer outH writeLines
--  lift (hClose inH)
--  lift (hClose outH)

--main :: IO ()
--main = do
--  arg1:arg2:_ <- getArgs
--  runEffect $ pipe arg1 arg2
{-# LANGUAGE ScopedTypeVariables #-}
module Data.ExternalMergeSort.Internal where

import           Control.Monad
import           Data.IORef
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import           Pipes
import           System.Environment
import           System.IO
import           Data.List
import           Data.Sequence (Seq, (|>) )
import qualified Data.Sequence as S
import           Data.Maybe


data MergeSortCfg a =
  MergeSortCfg {
      -- @readRec h@ is responsible for read one record from a handle
      readRec  :: Handle -> IO a
      -- @writeRec h r@ writes a single record to a handle
    , writeRec :: Handle -> a -> IO ()
    , ways :: Int -- the number of chunks the file will be broken into
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

mergeFiles :: Ord a => FilePath -> FilePath -> MergeSortCfg a -> IO ()
mergeFiles inF outF cfg = error "merge not implemented"

--mergeTwoP :: Ord a => Handle -> Handle -> Handle -> MergeSortCfg a -> Effect IO ()
--mergeTwoP inH inH' outH cfg = mergeReaderP cfg inH inH' >-> mergeRecsP >-> mergeWriterP cfg outH

--mergeReaderP :: Ord a => MergeSortCfg a -> Handle -> Handle -> Producer ([a],[a]) IO ()
--mergeReaderP cfg h h' = undefined
----  rs  <- lift $ replicateM (chunkSize cfg) (readRec cfg h)
----  rs' <- lift $ replicateM (chunkSize cfg) (readRec cfg h)
----  yield (rs,rs')

--mergeWriterP :: Ord a => MergeSortCfg a -> Handle -> Consumer [a] IO ()
--mergeWriterP cfg h = do
--  rs <- await
--  lift $ mapM_ (writeRec cfg h) rs

--merge :: Ord a => [a] -> [a] -> [a]
--merge []       ys   = ys
--merge xs       []   = xs
--merge (x:xs) (y:ys)
--  | x <= y          = x:merge xs     (y:ys)
--  | otherwise       = y:merge (x:xs) ys


data Buffer a b =
  Buffer
    { vec :: Vector a
    , idx :: Int
    , idnt :: b
    } deriving (Read, Show, Eq)

type KWayInput a b = Vector (Buffer a b)

data KWayMergeResult a b =
    -- ident of buffer that is empty
    InputBufferEmpty b (KWayInput a b) (Seq a)
  | OutputBuffer       (KWayInput a b) (Seq a)
  | Finished                           (Seq a)
  deriving (Read, Show, Eq)

--
-- @kMerge n bufs@ will perform a k-way merge (where k is the length of @bufs@)
-- on @bufs up until the point where one of the buffers in @bufs@ is empty OR
-- the output buffer is full
--
partialKMerge :: Ord a => Int -> Vector (Buffer a b) -> Seq a -> KWayMergeResult a b
partialKMerge outLen buffers out = go buffers out
  where
    foo :: Ord a => Vector (Buffer a b) -> Maybe b
    foo buffers = idnt <$> V.find (\buf -> idx buf >= V.length (vec buf)) buffers
    go buffers out
      | Just i <- foo buffers = InputBufferEmpty i buffers out
      | V.length buffers == 0 = Finished out
      | S.length out == outLen = OutputBuffer buffers out
      | otherwise =
         let vals         = bufferHeads buffers
             (m,minIndex) = minOf vals
             buf          = buffers ! minIndex
         in  go (buffers // [(minIndex, buf { idx = idx buf + 1}  )]) (out |> m)
    bufferHeads :: Ord a => Vector (Buffer a b) -> Vector a
    bufferHeads vss = V.map (\buf -> (vec buf)!(idx buf)) vss
    minOf :: Ord a => Vector a -> (a, Int)
    minOf vs = let m = minimum (V.toList vs)
               in  (m, fromJust $ V.findIndex (==m) vs)
--    size buffers = V.foldl (\t (v,i) -> V.length v - i + t) 0 $ buffers buffers

test n = partialKMerge n (V.fromList [ Buffer (V.fromList [1,2,3,4,5,6,7,8,9]) 0 'a',
                                     Buffer (V.fromList [3,5,7,9,11]) 0 'b' ]) S.empty

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
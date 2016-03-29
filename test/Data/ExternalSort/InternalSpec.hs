module Data.ExternalSort.InternalSpec (spec, main) where

import Data.ExternalSort.Internal
import TestUtil

import System.Directory
import System.IO
import System.IO.Temp   (withSystemTempDirectory)
import System.Random
import Test.Hspec

--
-- Although the library was written to sort files larger than memory for
-- these tests @numRecords@ must be small enough that the records read from the
-- file fit into memory
--

numRecords :: Int
numRecords = 100000

chunkSz :: Int
chunkSz = numRecords `div` 10

spec :: Spec
spec = do
    describe "Sorting works" $ do
      context "sortAndWriteToChunks" $ do
        it "Intermediate files are sorted" $ do
          withSystemTempDirectory "sort-spec-intermediate" $ \tmpDir -> do
            (path, h) <- genRandomFileAndOpen tmpDir numRecords
            let cfg = int32SortCfgOfSize chunkSz
            files <- sortAndWriteToChunks cfg tmpDir h
            results <- mapM (isFileSorted cfg) files
            -- clean up
            mapM_ removeFile files -- Shouldn't need to, but can't hurt.
            hClose h
            removeFile path
            -- check results
            all id results `shouldBe` True
      context "externalSortFile" $ do
        it "sorts the file" $ do
          withSystemTempDirectory "sort-spec-sorting" $ \tmpDir -> do
            inFile  <- genRandomFile tmpDir numRecords
            outFile <- genOutputFileName tmpDir
            let cfg =  int32SortCfgOfSize chunkSz
            externalSortFile cfg inFile outFile
            isSorted <- isFileSorted cfg outFile
            mapM_ removeFile [inFile, outFile]
            isSorted `shouldBe` True

main :: IO ()
main = hspec spec

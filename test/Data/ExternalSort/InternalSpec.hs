module Data.ExternalSort.InternalSpec where

import Data.ExternalSort.Internal
import TestUtil

import Data.IORef
import System.Directory
import System.IO
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
          (path, h) <- genRandomFileAndOpen numRecords
          fileRef <- newIORef []
          let cfg = int32SortCfgOfSize chunkSz
          sortAndWriteToChunks cfg fileRef h
          files <- readIORef fileRef
          results <- mapM (isFileSorted cfg) files
          -- clean up
          mapM_ removeFile files
          hClose h
          removeFile path
          -- check results
          all id results `shouldBe` True
      context "externalSortFile" $ do
        it "sorts the file" $ do
          inFile <- genRandomFile numRecords
          outFile <-genOutputFileName
          let cfg = int32SortCfgOfSize chunkSz
          externalSortFile cfg inFile outFile
          isSorted <- isFileSorted cfg outFile
          mapM_ removeFile [inFile, outFile]
          isSorted `shouldBe` True

main :: IO ()
main = hspec spec
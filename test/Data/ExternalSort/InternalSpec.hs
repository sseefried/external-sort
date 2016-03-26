module Data.ExternalSort.InternalSpec where

import Data.ExternalSort.Internal
import TestUtil

import Data.IORef
import System.Directory
import System.IO
import System.Random
import Test.Hspec

numRecords :: Int
numRecords = 10000

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

main :: IO ()
main = hspec spec
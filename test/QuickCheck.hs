{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Pipes
import qualified Pipes.Prelude as P
import System.IO
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.List

-- friends
import Data.ExternalMergeSort.Internal

cfg :: MergeSortCfg Int
cfg =
  MergeSortCfg {
    readRec  = \h -> read <$> hGetLine h
  , writeRec = \h -> hPutStrLn h . show
  , ways = 5
  }

---- test the mergeRecsP pipeline
--prop_MergeRecsP :: ([Int], [Int]) -> Property
--prop_MergeRecsP (xs,ys) = monadicIO $ do
--  as <- run mergedRecords
--  assert (sort (xs ++ ys) == as)
--  where
--    mergedRecords ::IO [Int]
--    mergedRecords = listProducerToList $ yield (sort xs,sort ys) >-> mergeRecsP

--listProducerToList :: Monad m => Producer [a] m () -> m [a]
--listProducerToList = P.fold (++) [] id

---- When @merge@ is presented with two sorted lists it produces a list that is still sorted
--prop_MergeKeepsListSorted :: [Int] -> [Int] -> Bool
--prop_MergeKeepsListSorted xs ys = merge (sort xs) (sort ys) == sort (xs ++ ys)

--prop_mergeMultipleChunks :: [[Int]] -> [[Int]] -> Bool
--prop_mergeMultipleChunks xss yss =
--  mergeMultiple (map sort xss) (map sort yss) == sort (concat $ xss ++ yss)

return []
main = $quickCheckAll





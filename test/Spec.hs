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
  , sizeRec  = \_ -> 1
  }

propFoo :: ([Int], [Int]) -> Property
propFoo (xs,ys) = monadicIO $ do
  as <- run mergedRecords
  assert (sort (xs ++ ys) == as)
  where
    mergedRecords ::IO [Int]
    mergedRecords = P.fold (++) [] id $ yield (xs,ys) >-> mergeRecsP


main :: IO ()
main = do
  quickCheck propFoo


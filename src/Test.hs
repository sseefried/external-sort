module Main where

import           Control.Monad
import           Data.List
import           Pipes
import qualified Pipes.Prelude as P

--
-- This example proves that you can "chunk" a stream so that a certain amount of
-- it is kept in memory at any one time and then "squeezed" back into a stream
--

chunkSize = 1000000

intChunks :: [[Int]]
intChunks = map (\n -> reverse [n*chunkSize,n*chunkSize+1..(n+1)*chunkSize - 1]) [0..1000]

effect :: Effect IO ()
effect = each intChunks >-> sorter >-> squeeze >-> putIt

sorter :: (Monad m, Ord a) => Pipe [a] [a] m r
sorter = do
  as <- await
  let as' = sort as
  yield as'
  sorter

squeeze :: Monad m => Pipe [a] a m r
squeeze = go
  where
    go = do
      xs <- await
      mapM_ yield xs
      go

putIt :: Show a => Consumer a IO ()
putIt = do
  a <- await
  lift (putStr $ show a ++ " ")
  putIt


main :: IO ()
main = runEffect effect
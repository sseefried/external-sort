module Main where

import           Control.Monad (when)
import           Data.ExternalMergeSort.Internal (externalMergeSort, MergeSortCfg(..))
import           System.IO
import           System.Environment
import           System.Exit



main :: IO ()
main = do
  args <- getArgs
  when (length args < 3) $ do
    putStrLn "Usage: sort-tester <in file> <out file> <chunk size>"
    exitWith (ExitFailure 1)
  let inFile:outFile:chunkSizeStr:_ = args
      cfg = MergeSortCfg reader write (read chunkSizeStr)
  externalMergeSort cfg inFile outFile


reader :: Handle -> IO Int
reader h = (read <$> hGetLine h)

write :: Show a => Handle -> a -> IO ()
write h = hPutStrLn h . show
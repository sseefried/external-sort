{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import           Control.Exception
import           Control.Monad (when, replicateM_)
import           Criterion.Main
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import           Data.ExternalSort.Internal (externalSortFile, ExternalSortCfg(..))
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Posix.Temp (mkstemp)
import           System.Random
import qualified Text.Show.ByteString as S

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) $ do
    putStrLn "Usage: sort-tester <number of ints> <chunk size>"
    exitWith (ExitFailure 1)
  let numberOfIntsStr:chunkSizeStr:restArgs = args
      cfg = ExternalSortCfg reader write (read chunkSizeStr)
  putStrLn "Generating random file..."
  inFile  <- genRandomFile (read numberOfIntsStr)
  outFile <- genOutputFileName
  finally
    (withArgs restArgs $
      defaultMain [
        bgroup "sort-tester" [
          bench "sort" $ nfIO (externalSortFile cfg inFile outFile)
        ]
      ])
    (do removeFile inFile
        removeFile outFile)

genRandomFile :: Int -> IO FilePath
genRandomFile n = do
  (path, inH) <- mkstemp "unsorted.txt."
  let writeInt = do
        (i :: Int) <- randomRIO (0,maxBound)
        B.hPutStrLn inH (LB.toStrict $ S.show i)
  replicateM_ n writeInt
  hClose inH
  return path

genOutputFileName :: IO FilePath
genOutputFileName = do
  (path, outH) <- mkstemp "sorted.txt."
  hClose outH
  return path

reader :: Handle -> IO Int
reader h = (read <$> hGetLine h)

write :: Show a => Handle -> a -> IO ()
write h = hPutStrLn h . show
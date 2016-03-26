{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import           TestUtil

import           Control.Exception
import           Control.Monad (when)
import           Criterion.Main
import           Data.ExternalSort (externalSortFile)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Posix.Temp (mkstemp)

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) $ do
    putStrLn "Usage: sort-tester <number of ints> <chunk size>"
    exitWith (ExitFailure 1)
  let numberOfIntsStr:chunkSizeStr:restArgs = args
  putStrLn "Generating random file..."
  inFile  <- genRandomFile (read numberOfIntsStr)
  outFile <- genOutputFileName
  finally
    (withArgs restArgs $
      defaultMain [
        bgroup "sort-tester" [
          bench "sort" $ nfIO (externalSortFile (int32SortCfgOfSize (read chunkSizeStr))
                                                 inFile outFile)
        ]
      ])
    (do removeFile inFile
        removeFile outFile)



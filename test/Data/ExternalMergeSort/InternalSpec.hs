module Data.ExternalMergeSort.InternalSpec where

import Data.ExternalMergeSort.Internal
import Test.Hspec

spec :: Spec
spec = do
    describe "first test" $ do
      context "some context" $ do
        it "1 == 1" $ do
          1 `shouldBe` 1
    describe "first test" $ do
      context "some context" $ do
        it "1 == 1" $ do
          2 `shouldBe` 2


main :: IO ()
main = hspec spec
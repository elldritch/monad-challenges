module Main (main) where

import Test.Hspec (hspec, describe, it, shouldBe)
import Set1 (fiveRands)

main :: IO ()
main = hspec $ do
  describe "Set 1: Random Numbers" $ do
    it "fiveRands" $ product fiveRands `shouldBe` 8681089573064486461641871805074254223660

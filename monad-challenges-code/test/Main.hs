{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Base16 (encode)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import MCPrelude (mkSeed)
import Set1 (fiveRands, randEven, randOdd, randPair, randString3, randTen, randPair_)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Set 1: Random Numbers" $ do
    it "fiveRands" $ product fiveRands `shouldBe` 8681089573064486461641871805074254223660
    it "randString3" $ encode (hash $ encodeUtf8 $ T.pack randString3) `shouldBe` "9d475eb78d3e38085220ed6ebde9d8f7d26540bb1c8f9382479c3acd4c8c94a3"
    it "rand{Even,Odd,Ten}" $ product ((\f -> fst $ f $ mkSeed 1) <$> [randEven, randOdd, randTen]) `shouldBe` 189908109902700
    it "randPair" $ fst (randPair $ mkSeed 1) `shouldBe` ('l', 282475249)
    it "randPair_" $ fst (randPair_ $ mkSeed 1) `shouldBe` ('l', 282475249)

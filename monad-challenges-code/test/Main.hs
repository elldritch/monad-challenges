{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (Maybe (..))

import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Base16 (encode)
import Data.Foldable (forM_)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec (describe, hspec, it, shouldBe)

import MCPrelude (greekDataA, greekDataB, mkSeed, salaries)

import Set1 (fiveRands, randEven, randOdd, randPair, randPair_, randString3, randString3', randTen)
import Set2 (Maybe (..), addSalaries, addSalaries2, queryGreek, queryGreek2, tailMax, tailMin, tailProd, tailSum)

main :: IO ()
main = hspec $ do
  describe "Set 1: Random Numbers" $ do
    it "fiveRands" $ product fiveRands `shouldBe` 8681089573064486461641871805074254223660
    it "randString3" $ encode (hash $ encodeUtf8 $ T.pack randString3) `shouldBe` "9d475eb78d3e38085220ed6ebde9d8f7d26540bb1c8f9382479c3acd4c8c94a3"
    it "rand{Even,Odd,Ten}" $ product ((\f -> fst $ f $ mkSeed 1) <$> [randEven, randOdd, randTen]) `shouldBe` 189908109902700
    it "randPair" $ fst (randPair $ mkSeed 1) `shouldBe` ('l', 282475249)
    it "randPair_" $ fst (randPair_ $ mkSeed 1) `shouldBe` ('l', 282475249)
    it "randString3'" $ encode (hash $ encodeUtf8 $ T.pack $ fst randString3') `shouldBe` "9d475eb78d3e38085220ed6ebde9d8f7d26540bb1c8f9382479c3acd4c8c94a3"

  describe "Set 2: Failing Computations" $ do
    describe "queryGreek" $ testQueryGreek queryGreek
    describe "queryGreek2" $ testQueryGreek queryGreek2
    describe "addSalaries" $ testAddSalaries addSalaries
    describe "addSalaries2" $ testAddSalaries addSalaries2
    it "tailProd" $ do
      tailProd [] `shouldBe` Nothing
      tailProd [42] `shouldBe` Just 1
      tailProd [2, 3, 5] `shouldBe` Just 15
    it "tailSum" $ do
      tailSum [] `shouldBe` Nothing
      tailSum [42] `shouldBe` Just 0
      tailSum [2, 3, 5] `shouldBe` Just 8
    it "tailMax" $ do
      tailMax [] `shouldBe` (Nothing :: Maybe (Maybe Integer))
      tailMax [42] `shouldBe` Just Nothing
      tailMax [2, 3, 5] `shouldBe` Just (Just 5)
    it "tailMin" $ do
      tailMin [] `shouldBe` (Nothing :: Maybe (Maybe Integer))
      tailMin [42] `shouldBe` Just Nothing
      tailMin [2, 3, 5] `shouldBe` Just (Just 3)
 where
  testQueryGreek f =
    forM_ cases $
      \(key, (dict, expected)) -> it key $ f dict key `shouldBe` expected
   where
    cases =
      [ ("alpha", (greekDataA, Just 2.0))
      , ("beta", (greekDataA, Nothing))
      , ("gamma", (greekDataA, Just 3.3333333333333335))
      , ("delta", (greekDataA, Nothing))
      , ("zeta", (greekDataA, Nothing))
      , ("rho", (greekDataB, Nothing))
      , ("phi", (greekDataB, Just 0.24528301886792453))
      , ("chi", (greekDataB, Just 9.095238095238095))
      , ("psi", (greekDataB, Nothing))
      , ("omega", (greekDataB, Just 24.0))
      ]

  testAddSalaries f = forM_ cases $ \(a, b, expected) -> it (show (a, b)) $ f salaries a b `shouldBe` expected
   where
    cases =
      [ ("alice", "alice", Just 210000)
      , ("alice", "bob", Just 195000)
      , ("alice", "someone", Nothing)
      , ("someone", "else", Nothing)
      ]

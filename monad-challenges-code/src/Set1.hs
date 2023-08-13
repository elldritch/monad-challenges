module Set1 (
  fiveRands,
  randString3,
  randEven,
  randOdd,
  randTen,
  randPair,
  randPair_,
  randString3',
) where

import MCPrelude (
  Char,
  Integer,
  Seed,
  String,
  foldr,
  fromInteger,
  mkSeed,
  rand,
  replicate,
  toLetter,
  ($),
  (*),
  (+),
  (++),
  (.),
 )

-- Part 1

fiveRands :: [Integer]
fiveRands = [n1, n2, n3, n4, n5]
 where
  s0 = mkSeed 1
  (n1, s1) = rand s0
  (n2, s2) = rand s1
  (n3, s3) = rand s2
  (n4, s4) = rand s3
  (n5, _) = rand s4

-- Part 2

randLetter :: Seed -> (Char, Seed)
randLetter s = (toLetter n, s')
 where
  (n, s') = rand s

randString3 :: String
randString3 = [c1, c2, c3]
 where
  s0 = mkSeed 1
  (c1, s1) = randLetter s0
  (c2, s2) = randLetter s1
  (c3, _) = randLetter s2

-- Part 3

type Gen t = Seed -> (t, Seed)

randEven :: Gen Integer
randEven = generalA (* 2)

randOdd :: Gen Integer
randOdd = generalA $ (+ 1) . (* 2)

randTen :: Gen Integer
randTen = generalA (* 10)

generalA :: (Integer -> a) -> Seed -> (a, Seed)
generalA f s = (f n, s')
 where
  (n, s') = rand s

-- Part 4

randPair :: Gen (Char, Integer)
randPair s0 = ((c, n), s2)
 where
  (c, s1) = randLetter s0
  (n, s2) = rand s1

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb s0 = ((a, b), s2)
 where
  (a, s1) = ga s0
  (b, s2) = gb s1

randPair_ :: Gen (Char, Integer)
randPair_ = generalPair randLetter rand

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb s0 = (f a b, s2)
 where
  (a, s1) = ga s0
  (b, s2) = gb s1

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

-- Part 5

repRandom :: [Gen a] -> Gen [a]
repRandom gs s0 = foldr f ([], s0) gs
 where
  f :: Gen a -> ([a], Seed) -> ([a], Seed)
  f ga (acc, s) = (acc ++ [x], s')
   where
    (x, s') = ga s

randString3' :: (String, Seed)
randString3' = repRandom (replicate 3 randLetter) (mkSeed 1)

-- Part 6

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f s0 = (b, s2)
 where
  (a, s1) = ga s0
  gb = f a
  (b, s2) = gb s1

mkGen :: a -> Gen a
mkGen x s = (x, s)

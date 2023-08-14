module Set4 (randString3'') where

import MCPrelude

-- Part 2

type Gen t = Seed -> (t, Seed)

mkGen :: a -> Gen a
mkGen x s = (x, s)

generalA :: (Integer -> a) -> Gen a
generalA f s = (f n, s')
 where
  (n, s') = rand s

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f s0 = (b, s2)
 where
  (a, s1) = ga s0
  gb = f a
  (b, s2) = gb s1

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = ga `genTwo` \a -> gb `genTwo` \b -> mkGen $ f a b

repRandom :: [Gen a] -> Gen [a]
repRandom [] = mkGen []
repRandom (g : gs) = g `genTwo` \a -> repRandom gs `genTwo` \as -> mkGen $ a : as

randLetter :: Seed -> (Char, Seed)
randLetter s = (toLetter n, s')
 where
  (n, s') = rand s

randString3'' :: (String, Seed)
randString3'' = repRandom (replicate 3 randLetter) (mkSeed 1)

-- Part 3

module Set1
  ( fiveRands,
    randString3,
    randEven,
    randOdd,
    randTen,
    randPair,
    randPair_,
  )
where

import MCPrelude (Seed, mkSeed, rand, toLetter)

type Gen t = Seed -> (t, Seed)

randsFromGen :: Gen t -> Seed -> [(t, Seed)]
randsFromGen f seed = tail $ iterate (\(x, s) -> f s) (undefined, seed)

rands :: Seed -> [(Integer, Seed)]
rands = randsFromGen rand

fiveRands :: [Integer]
fiveRands = take 5 $ fmap fst $ rands $ mkSeed 1

randLetter :: Gen Char
randLetter seed = let (n, s) = rand seed in (toLetter n, s)

randString3 :: String
randString3 = take 3 $ fmap fst $ randsFromGen randLetter $ mkSeed 1

randEven :: Gen Integer
randEven = generalA (* 2) rand

randOdd :: Gen Integer
randOdd = generalA (+ 1) randEven

randTen :: Gen Integer
randTen = generalA (* 10) rand

generalA :: (a -> b) -> Gen a -> Seed -> (b, Seed)
generalA f r s = let (x, s') = r s in (f x, s')

randPair :: Gen (Char, Integer)
randPair s = ((c, n), s'')
  where
    (c, s') = randLetter s
    (n, s'') = rand s'

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair f g s = ((a, b), s'')
  where
    (a, s') = f s
    (b, s'') = g s'

randPair_ :: Gen (Char, Integer)
randPair_ = generalPair randLetter rand

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f g1 g2 s = (f a b, s'')
  where
    ((a, b), s'') = generalPair g1 g2 s

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

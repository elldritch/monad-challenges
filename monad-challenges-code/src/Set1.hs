module Set1 (fiveRands, randString3) where

import MCPrelude (Seed, mkSeed, rand, toLetter)

randsFromGen :: (Seed -> (t, Seed)) -> Seed -> [(t, Seed)]
randsFromGen f seed = tail $ iterate (\(x, s) -> f s) (undefined, seed)

rands :: Seed -> [(Integer, Seed)]
rands = randsFromGen rand

fiveRands :: [Integer]
fiveRands = take 5 $ fmap fst $ rands $ mkSeed 1

randLetter :: Seed -> (Char, Seed)
randLetter seed = let (n, s) = rand seed in (toLetter n, s)

randString3 :: String
randString3 = take 3 $ fmap fst $ randsFromGen randLetter $ mkSeed 1

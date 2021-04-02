module Set1 (fiveRands) where

import MCPrelude (mkSeed, rand)

fiveRands :: [Integer]
fiveRands = [a, b, c, d, e]
  where
    (a, a') = rand $ mkSeed 1
    (b, b') = rand a'
    (c, c') = rand b'
    (d, d') = rand c'
    (e, e') = rand d'

module Set4 (randString3'') where

import MCPrelude
import Set2 (Maybe, link, mkMaybe)

-- Part 2

mkGen :: a -> Gen a
mkGen x = Gen $ \s -> (x, s)

generalA :: (Integer -> a) -> Gen a
generalA f = Gen $ \s -> let (n, s') = rand s in (f n, s')

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo (Gen ga) f = Gen $ \s0 ->
  let
    (a, s1) = ga s0
    (Gen gb) = f a
    (b, s2) = gb s1
   in
    (b, s2)

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = ga `genTwo` \a -> gb `genTwo` \b -> mkGen $ f a b

repRandom :: [Gen a] -> Gen [a]
repRandom [] = mkGen []
repRandom (g : gs) = g `genTwo` \a -> repRandom gs `genTwo` \as -> mkGen $ a : as

randLetter :: Gen Char
randLetter = Gen $ \s -> let (n, s') = rand s in (toLetter n, s')

randString3'' :: (String, Seed)
randString3'' = g $ mkSeed 1
 where
  (Gen g) = repRandom $ replicate 3 randLetter

-- Part 3

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

-- Part 4

newtype Gen t = Gen (Seed -> (t, Seed))

evalGen :: Gen a -> Seed -> a
evalGen (Gen g) = fst . g

instance Monad Gen where
  bind = genTwo
  return = mkGen

instance Monad Maybe where
  bind = link
  return = mkMaybe

instance Monad [] where
  bind [] _ = []
  bind (x : xs) f = f x ++ bind xs f
  return x = [x]

-- Part 5

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (mx : mxs) = mx `bind` \x -> sequence mxs `bind` \xs -> return $ x : xs

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = ma `bind` \a -> mb `bind` \b -> return $ f a b

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = ma `bind` \a -> mb `bind` \b -> mc `bind` \c -> return $ f a b c

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

join :: Monad m => m (m a) -> m a
join mmx = mmx `bind` id

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = mf `bind` \f -> ma `bind` \a -> return $ f a

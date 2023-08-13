module Set3 (
  allPairs,
  allCards,
  allPairs',
  allCards',
  allCombs3,
  allCombs',
  allCombs3',
) where

import MCPrelude

-- Part 1

allPairs :: [a] -> [b] -> [(a, b)]
allPairs [] _ = []
allPairs (a : as) bs = go a bs ++ allPairs as bs
 where
  go :: a -> [b] -> [(a, b)]
  go _ [] = []
  go x (y : ys) = (x, y) : go x ys

-- Part 2

data Card = Card Int String

instance Show Card where
  show (Card rank suit) = show rank ++ suit

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards (a : as) bs = go a bs ++ allCards as bs
 where
  go :: Int -> [String] -> [Card]
  go _ [] = []
  go a' (b' : bs') = Card a' b' : go a' bs'

-- Part 3

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs f (a : as) bs = go a bs ++ allCombs f as bs
 where
  go _ [] = []
  go a' (b' : bs') = f a' b' : go a' bs'

allPairs' :: [a] -> [b] -> [(a, b)]
allPairs' = allCombs (,)

allCards' :: [Int] -> [String] -> [Card]
allCards' = allCombs Card

-- Part 4

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ [] _ _ = []
allCombs3 f (a : as) bs cs = go a bs cs ++ allCombs3 f as bs cs
 where
  go a' (b' : bs') cs' = go' a' b' cs' ++ go a' bs' cs'
  go _ [] _ = []

  go' _ _ [] = []
  go' a'' b'' (c'' : cs'') = f a'' b'' c'' : go' a'' b'' cs''

-- Part 5

combStep :: [a -> b] -> [a] -> [b]
combStep (f : fs) xs = go f xs ++ combStep fs xs
 where
  go :: (a -> b) -> [a] -> [b]
  go _ [] = []
  go f' (x' : xs') = f' x' : go f' xs'
combStep _ _ = []

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f as bs = concat $ combStep [\a -> combStep [f a] bs] as

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f as bs cs = concat $ concat $ combStep [\a -> combStep [\b -> combStep [f a b] cs] bs] as

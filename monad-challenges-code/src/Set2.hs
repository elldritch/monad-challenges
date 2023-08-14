module Set2 (
  Maybe (..),
  link,
  yLink,
  mkMaybe,
  queryGreek,
  queryGreek2,
  addSalaries,
  addSalaries2,
  tailProd,
  tailSum,
  tailMax,
  tailMin,
) where

import MCPrelude

-- Part 1

data Maybe t = Just t | Nothing

instance Show a => Show (Maybe a) where
  show (Just t) = "Just " ++ show t
  show Nothing = "Nothing"

instance Eq a => Eq (Maybe a) where
  Just a == Just b = a == b
  Nothing == Nothing = True
  _ == _ = False

-- Part 2

headMay :: [a] -> Maybe a
headMay (x : _) = Just x
headMay _ = Nothing

tailMay :: [a] -> Maybe [a]
tailMay (_ : x) = Just x
tailMay _ = Nothing

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay x ((a, b) : xs) = if a == x then Just b else lookupMay x xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay a b = Just $ a / b

maximumMay :: Ord a => [a] -> Maybe a
maximumMay = foldr f Nothing
 where
  f :: Ord a => a -> Maybe a -> Maybe a
  f x Nothing = Just x
  f x (Just y) = Just $ max x y

minimumMay :: Ord a => [a] -> Maybe a
minimumMay = foldr f Nothing
 where
  f :: Ord a => a -> Maybe a -> Maybe a
  f x Nothing = Just x
  f x (Just y) = Just $ min x y

-- Part 3

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d key = case xsHead of
  Nothing -> Nothing
  Just h -> case xsTailMax of
    Nothing -> Nothing
    Just t -> divMay (fromIntegral t) (fromIntegral h)
 where
  xs = lookupMay key d
  xsTail = case xs of
    Just ns -> tailMay ns
    Nothing -> Nothing
  xsTailMax = case xsTail of
    Just ns -> maximumMay ns
    Nothing -> Nothing
  xsHead = case xs of
    Just ns -> headMay ns
    Nothing -> Nothing

-- Part 4

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f ma = case ma of
  Nothing -> Nothing
  Just a -> f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 d key = xsTailMax `link` \t -> xsHead `link` \h -> divMay (fromIntegral t) (fromIntegral h)
 where
  xs = lookupMay key d
  xsTailMax = xs `link` tailMay `link` maximumMay
  xsHead = xs `link` headMay

-- Part 5

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries d a b = mx `link` \x -> my `link` \y -> mkMaybe $ x + y
 where
  mx = lookupMay a d
  my = lookupMay b d

yLink :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
yLink f ma mb = ma `link` \a -> mb `link` \b -> f a b

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 d a b = yLink (\x y -> mkMaybe $ x + y) mx my
 where
  mx = lookupMay a d
  my = lookupMay b d

mkMaybe :: a -> Maybe a
mkMaybe = Just

-- Part 6

tailProd :: Num a => [a] -> Maybe a
tailProd xs = tailMay xs `link` (mkMaybe . product)

tailSum :: Num a => [a] -> Maybe a
tailSum xs = tailMay xs `link` (mkMaybe . sum)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f ma = ma `link` (mkMaybe . f)

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax = transMaybe maximumMay . tailMay

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin = transMaybe minimumMay . tailMay

combine :: Maybe (Maybe a) -> Maybe a
combine mm = case mm of
  Nothing -> Nothing
  Just m -> m

{-|
Module      : Homework1Tasks
Description : Solutions to homework 1 tasks
Maintainer  : Dinko Osrecki
-}
module Homework1Tasks where

import           Data.Char
import           Data.Set  (Set, fromList, member, toList, union)

-- | 1.1
--   Write a function that checks if a year is a leap year.
isLeapYear :: Int -> Bool
isLeapYear y = divisibleBy 4 && (not(divisibleBy 100) || divisibleBy 400)
  where divisibleBy d = y `rem` d == 0

-- | 1.2
--   Generate a list of all leap years between 1996 and 2017.
leapList :: [Int]
leapList = filter isLeapYear [1996..2017]

-- | 2. Implement the exponential function
-- | 2. a)
--   Write a function which evaluates polynomial of single indeterminante
--   'x' where polynomial coefficients are represented by a list of Doubles.
evaluate :: Double -> [Double] -> Double
evaluate x as = sum $ zipWith term ks as
  where
    term k a = a * (x ** k)
    ks       = [0..]

-- | 2. b)
--   Define a function that calculates the nth factorial.
factorial :: Double -> Double
factorial n
  | n >= 0    = product [1..n]
  | otherwise = error "argument must be non-negative"

-- | 2. c)
--   Define an infinite list of coefficients for Maclaurin series.
maclaurin :: [Double]
maclaurin = map (\n -> 1 / factorial n) [0..]

-- | 2. d)
--   Combine evaluate, factorial and maclaurin to create a function
--   which approximates exp from the first 170 terms of the series.
exp' :: Double -> Double
exp' n = evaluate n $ take 170 maclaurin

-- | 3. Implement some polymorphic utility functions for working on
--   lists of key value pairs where keys are strings.
-- | 3. a)
--   Implement a function for getting a pair with a certain key which
--   should return a list with a single element as a result (or no
--   elements if key doesn’t exist).
findItem :: [(String, a)] -> String -> [(String, a)]
findItem [] _ = []
findItem (x:xs) k
  | k == fst x = [x]
  | otherwise  = findItem xs k

-- | 3. b)
--   Implement a function that checks if a list contains an element
--   with a certain key.
contains :: [(String, a)] -> String -> Bool
contains xs k = not $ null $ findItem xs k

-- | 3. c)
--   Implement a function that tries to retrieve a value with a certain\
--   key or throws an error if the key does not exist.
lookup' :: [(String, a)] -> String -> a
lookup' xs k
  | contains xs k = snd $ head $ findItem xs k
  | otherwise     = error "key does not exist"

-- | 3. d)
--   Implement a function that inserts a new key value pair. If the key
--   already exists then do nothing.
insert :: [(String, a)] -> (String, a) -> [(String, a)]
insert xs y@(k, _)
  | contains xs k = xs
  | otherwise     = y:xs

-- | 3. e)
--   Implement a function that removes a key value pair with the
--   certain key.
remove :: [(String, a)] -> String -> [(String, a)]
remove [] _ = []
remove (x:xs) k
  | fst x == k = xs
  | otherwise  = x : remove xs k

-- | 3. f)
--   Implement a function that updates the value of a certain key
--   (if the key does not exist, function does nothing).
update :: [(String, a)] -> String -> a -> [(String, a)]
update [] _ _ = []
update (x:xs) k v
  | fst x == k = (k, v):xs
  | otherwise  = x : update xs k v

-- | 4.
--   Implement a function that calculates a cosine similarity between
--   two texts.
bag :: String -> Set String
bag = fromList . words . map toLower . filter valid
  where
    valid c = isLetter c || isSpace c

space :: Set String -> Set String -> [String]
space xs ys = toList $ union xs ys

vector :: Set String -> [String] -> [Int]
vector b = map (boolToInt . (`member` b))
  where
    boolToInt x = if x then 1 else 0

dotProduct :: [Int] -> [Int] -> Double
dotProduct xs ys = fromIntegral $ sum $ zipWith (*) xs ys

magnitude :: [Int] -> Double
magnitude = sqrt . sum . map ((** 2) . fromIntegral)

similarity :: [Int] -> [Int] -> Double
similarity xs ys = dotProduct xs ys / (magnitude xs * magnitude ys)

cosineSimilarity :: String -> String -> Double
cosineSimilarity a b = similarity (vector m s) (vector n s)
  where
    s = space m n
    m = bag a
    n = bag b

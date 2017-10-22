{-|
Module      : Lecture2Exercises
Description : Solutions to in-class exercises for Lecture 2
Maintainer  : dinko.osrecki@gmail.com
-}
module Lecture2Exercises where

import           Data.Char
import           Data.List

-- | 1.1
--   Define a function that returns a list without the first three and the
--   last three elements.
noFirst3NoLast3 :: [a] -> [a]
noFirst3NoLast3 xs = zipWith (curry fst) (drop 3 xs) (drop 6 xs)

-- | 1.2
--   Define a function that takes a person's name and a surname and returns
--   a string consisting of person's initials.
initials :: String -> String -> String
initials name surname = [n, '.', ' ', s, '.']
  where
    (n:_) = name
    (s:_) = surname

-- | 1.3
--   Define a function that concatenates two strings, so that the longest
--   string always comes first.
concatenate :: [a] -> [a] -> [a]
concatenate a b = s ++ f
  where
    [f, s] = sortBy compareLength [a, b]

compareLength :: [a] -> [b] -> Ordering
compareLength [] []         = EQ
compareLength (_:_) []      = GT
compareLength [] (_:_)      = LT
compareLength (_:xs) (_:ys) = compareLength xs ys

-- | 1.4
--   Define a function that returns an empty list if input is an empty list,
--   otherwise it returns its first element wrapped inside a singleton list.
safeHead :: [a] -> [a]
safeHead []    = []
safeHead (x:_) = [x]

-- | 1.5
--   Define a function that checks whether a list contains duplicate
--   elements (use 'nub').
hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates xs = compareLength xs (nub xs) /= EQ

-- | 2.1
--   Define that generates a sequence [a*2, (a+1)*2, ..., b*2] and also
--   works when b<a.
doublesFromTo :: (Integral a) => a -> a -> [a]
doublesFromTo a b
  | a <= b    = [2 * x | x <- [a..b]]
  | otherwise = [2 * x | x <- [a,a-1..b]]

-- | 2.2
--   Redefine 'ceasarCode n xs' so that it shifts all letters a specified
--   number of positions 'n', converts all input to lowercase, and ensures
--   that letters remain within the ['a'..'z'] interval.
ceasarCode :: Int -> String -> String
ceasarCode n = map (shiftChar n . toLower)

shiftChar :: Int -> Char -> Char
shiftChar 0 c = c
shiftChar n c = shiftChar (n - 1) (next c)
  where
    next 'z' = 'a'
    next a   = succ a

-- | 3.1
--   Define a function that computes the total number of letters in a string,
--   thereby ignoring the whitespaces and all words shorter than three letters.
letterCount :: String -> Int
letterCount xs = sum [length ys | ys <- words xs, length ys > 2]

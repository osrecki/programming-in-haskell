{-|
Module      : Lecture8Exercises
Description : Solutions to in-class exercises for Lecture 8
Maintainer  : Dinko Osrecki
-}
module Lecture8Exercises where

import           Control.Arrow
import           Data.Char

-- | 1
--   Define the following functions using composition and
--   point-free style.
-- | 1.1
--   Define a function that adds up elements occurring at even
--   (incl. zero) positions in a list.
sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . enumerate

-- | Zip with index.
enumerate :: [a] -> [(Integer,a)]
enumerate = zip [0..]

-- | 1.2
--   Define a function that given a string 's' and a list 'ws',
--   removes from the string all words contained in the list.
filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

-- | 1.3 a
--   Define 'initials3 d p s' that takes a string 's' and turns
--   it into a string of initials. The function delimits the initials
--   with string 'd' and discards the initials of words that don't
--   satisfy the predicate 'p'.
initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = concatMap initial . filter p . words
  where
    initial = (:d) . toUpper . head

-- | 1.3 b
--   Use 'initials3' to define 'initials' function.
initials :: String -> String
initials = initials3 "." (const True)

-- | 2.1 a
--   Define a function that returns the maximum difference
--   between consecutive elements in the list.
maxDiff :: [Int] -> Int
maxDiff xs = maximum . map abs . zipWith (-) xs $ tail xs

-- | 2.1 b
--   Define a function that returns the pair (min_diff, max_diff).
minMaxDiff :: [Int] -> (Int,Int)
minMaxDiff xs = (minimum &&& maximum) diffs
  where
    diffs = map abs . zipWith (-) xs $ tail xs

-- 'maxDiff' by using 'minMaxDiff'
maxDiff' :: [Int] -> Int
maxDiff' = snd . minMaxDiff

-- | 2.2
--   Define a function 'studentsPassed' that given a list [(Name,Score)],
--   returns the names of all students who scored at least 50% of the
--   maximum score.
studentsPassed :: [(String,Double)] -> [String]
studentsPassed xs = map fst . filter ((>= threshold) . snd) $ xs
  where
    threshold = (/2) . maximum . map snd $ xs

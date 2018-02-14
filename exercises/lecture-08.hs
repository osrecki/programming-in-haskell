{-|
Module      : Lecture8Exercises
Description : Solutions to Lecture 8 exercises
Maintainer  : Dinko Osrecki
-}
module Lecture8Exercises where

import           Control.Arrow
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Ord

-- EXERCISE 01 ----------------------------------------------------------------

{-
  1
  - Define the following functions using composition and point-free style.

  1.1
  - Define a function which adds up elements occurring at even (incl. zero)
    positions in a list.
-}
sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . enumerate

-- zip with index
enumerate :: [a] -> [(Integer,a)]
enumerate = zip [0..]

{-
  1.2
  - Define a function which given a string 's' and a list 'ws', removes from
    the string all words contained in the list.
-}
filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

{-
  1.3 a
  - Define 'initials3 d p s' which takes a string 's' and turns it into a
    string of initials. The function delimits the initials with string 'd' and
    discards the initials of words that do not satisfy the predicate 'p'.
-}
initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = concatMap initial . filter p . words
  where
    initial = (:d) . toUpper . head

{-
  1.3 b
  - Use 'initials3' to define 'initials' function.
-}
initials :: String -> String
initials = initials3 "." (const True)

-- EXERCISE 02 ----------------------------------------------------------------

{-
  2.1 a
  - Define a function which returns the maximum difference between consecutive
    elements in the list.
-}
maxDiff :: [Int] -> Int
maxDiff xs = maximum . map abs . zipWith (-) xs $ tail xs

{-
  2.1 b
  - Define a function which returns the pair (min_diff, max_diff).
-}
minMaxDiff :: [Int] -> (Int,Int)
minMaxDiff xs = (minimum &&& maximum) diffs
  where
    diffs = map abs . zipWith (-) xs $ tail xs

-- 'maxDiff' by using 'minMaxDiff'
maxDiff' :: [Int] -> Int
maxDiff' = snd . minMaxDiff

{-
  2.2
  - Define a function 'studentsPassed' that given a list [(Name,Score)],
    returns the names of all students who scored at least 50% of the maximum
    score.
-}
studentsPassed :: [(String,Double)] -> [String]
studentsPassed xs = map fst . filter ((>= threshold) . snd) $ xs
  where
    threshold = (/2) . maximum . map snd $ xs

-- EXERCISE 03 ----------------------------------------------------------------

{-
  3.1
  - Define a function which checks whether every word in a string is
    capitalized.
-}
isTitleCased :: String -> Bool
isTitleCased = all isUpper . map head . words

{-
  3.2
  - Define a function which sorts the list of pairs in ascending order with
    respect to the second element of a pair.
-}
sortPairs :: (Ord b) => [(a,b)] -> [(a,b)]
sortPairs = sortBy (comparing snd)

sortPairs' :: (Ord b) => [(a,b)] -> [(a,b)]
sortPairs' = sortBy (compare `on` snd)

{-
  3.3
  - Define a function which extracts the the name of the file from a file path.

    Example:
    >>> filename "/etc/init/cron.conf"
    cron.conf
-}
filename :: String -> String
filename = reverse . takeWhile (/= '/') . reverse

{-
  3.4
  - Define a function which returns the indices of the maximum element in a
    list. Return "empty list" error if the list is empty.
-}
maxElemIndices :: (Ord a) => [a] -> [Int]
maxElemIndices xs = elemIndices (maximum xs) xs

-- EXERCISE 04 ----------------------------------------------------------------

{-
  4.1
  - Define 'elem' using 'foldr'.
-}
elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldr (\x acc -> acc || x == y) False

{-
  4.2
  - Define 'reverse' using 'foldr'.
-}
reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

-- more performant definition
reverse'' :: [a] -> [a]
reverse'' xs = foldr (\x acc -> acc . (x:)) id xs []

{-
  4.3
  - Using 'foldr' define 'nubRuns' that removes consecutively repeated elements
    from a list.
-}
nubRuns :: Eq a => [a] -> [a]
nubRuns = foldr prependElem []
  where
    prependElem x [] = [x]
    prependElem x ys@(y:_)
      | x == y    = ys
      | otherwise = x:ys

-- EXERCISE 05 ----------------------------------------------------------------

{-
  5.1
  - Write 'reverse' using 'foldl'.
-}
reverse''' :: [a] -> [a]
reverse''' = foldl (flip (:)) []

{-
  5.2
  - Using 'foldl' define function 'sumEven' from task 1.1.
-}
sumEven' :: (Num a) => [a] -> a
sumEven' = snd . foldl add (0, 0)
  where
    add :: (Num a) => (Int,a) -> a -> (Int,a)
    add (i,acc) x
      | even i    = (i+1, acc+x)
      | otherwise = (i+1, acc)

{-
  5.3
  - Using 'foldl' define a function which, given a list of tuples, returns the
    maximum element at the 1st position in a pair and the maximum element at
    the 2nd position in the pair. Return "empty list" error if the list is
    empty.
-}
maxUnzip :: (Bounded a, Ord a) => [(a,a)] -> (a,a)
maxUnzip [] = error "empty list"
maxUnzip xs = foldl maxTuple (minBound,minBound) xs
  where
    maxTuple (m1,m2) (x,y) = (max m1 x, max m2 y)

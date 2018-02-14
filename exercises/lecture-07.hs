{-|
Module      : Lecture7Exercises
Description : Solutions to Lecture 7 exercises
Maintainer  : Dinko Osrecki
-}
module Lecture7Exercises where

import           Control.Arrow
import           Data.Tuple

-- EXERCISE 01 ----------------------------------------------------------------

{-
  1
  - Define the functions using partial application of existing functions.

  1.1 a
  - Define a function which takes first three elements from the list.
-}
takeThree :: [a] -> [a]
takeThree = take 3

{-
  1.1 b
  - Define a function which drops first three elements from the list.
-}
dropThree :: [a] -> [a]
dropThree = drop 3

{-
  1.1 c
  - Define a function which takes one element and repeats it 100 times
    in a list.
-}
hundredTimes :: a -> [a]
hundredTimes = replicate 100

{-
  1.2 a
  - Define a function which indexes the elements in a list.
-}
index :: (Num b, Enum b) => [a] -> [(b,a)]
index = zip [0..]

{-
  1.2 b
  - Write the function which does the same, but with index at the 2nd position.
-}
index' :: (Num b, Enum b) => [a] -> [(a,b)]
index' = zipWith swap' [0..]
  where swap' = curry swap

{-
  1.3
  - Define a function which returns a string of length 'n' consisting of
    characters '='.
-}
divider :: Int -> String
divider = replicate' '='
  where replicate' = flip replicate

-- EXERCISE 02 ----------------------------------------------------------------

{-
  2.1 a
  - Define a function which applies a binary function on the last elements of
    two lists.
-}
applyOnLast :: (a -> b -> c) -> [a] -> [b] -> c
applyOnLast f xs ys = f (last xs) (last ys)

{-
  2.1 b
  - Using 'applyOnLast' and 'addThree' define a function which adds last
    elements of the lists and 100.
-}
lastTwoPlus100 :: (Num a) => [a] -> [a] -> a
lastTwoPlus100 = applyOnLast (addThree 100)

addThree :: Num a => a -> a -> a -> a
addThree a b c = a + b + c

{-
  2.2 a
  - Define a function which applies 'n' times function 'f' to argument 'x'.
    If n <= 0, return 'x' unaltered.
-}
applyManyTimes :: (Num a, Ord a) => a -> (b -> b) -> b -> b
applyManyTimes n f x
  | n <= 0    = x
  | otherwise = applyManyTimes (n - 1) f (f x)

{-
  2.2 b
  - Using 'applyManyTimes' define 'applyTwice'.
-}
applyTwice :: (a -> a) -> a -> a
applyTwice = applyManyTimes 2

-- EXERCISE 03 ----------------------------------------------------------------

{-
  3
  - Define the following functions using 'map'.

  3.1
  - Define a function which turns a list into list of lists containing each
    element.
-}
listifylist :: [a] -> [[a]]
listifylist = map (:[])

{-
  3.2
  - Define a function which cuts off all values from the list at the value 'n'.
-}
cutoff :: (Num a, Ord a) => a -> [a] -> [a]
cutoff n = map (min n)

-- EXERCISE 04 ----------------------------------------------------------------

{-
  4
  - Define the following functions using 'map' and 'filter'.

  4.1
  - Define a function which adds the squares of all even numbers from a list.
-}
sumEvenSquares :: (Integral a) => [a] -> a
sumEvenSquares = sum . map (^ 2) . filter even

{-
  4.2
  - Define a function which counts how many times given element occurs in
    the list.
-}
freq :: Eq a => a -> [a] -> Int
freq x = length . filter (== x)

{-
  4.3
  - Define a function which filters all elements that occur at least 'n'
    times in a list.
-}
freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = filter (\x -> freq x xs >= n) xs

-- EXERCISE 05 ----------------------------------------------------------------

{-
  5
  - Define the following functions using lambda expressions.

  5.1
  - Define a function which given a list, filters all elements that fall
    within the specified interval.
-}
withinInterval :: (Num a, Ord a) => a -> a -> [a] -> [a]
withinInterval n m = filter (\x -> n <= x && x <= m)

{-
  5.2
  - Define a function which returns the second column of the matrix, encoded
    as a list of lists.
-}
sndColumn :: [[a]] -> [a]
sndColumn = map (\row -> row !! 1)

sndColumn' :: [[a]] -> [a]
sndColumn' = map (!! 1)

{-
  5.3
  - Define a function which takes a list of pairs and returns a list of the
    pairs so that the first element in the pair is smaller than the second one.
    If elements are equal, the pair is discarded.
-}
canonicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
canonicalizePairs = map (\x -> (min' x, max' x)) . filter (\(a,b) -> a /= b)
  where
    min' = uncurry min
    max' = uncurry max

canonicalizePairs' :: Ord a => [(a, a)] -> [(a, a)]
canonicalizePairs' = map (uncurry min &&& uncurry max) . filter (uncurry (/=))

{-|
Module      : Lecture7Exercises
Description : Solutions to in-class exercises for Lecture 7
Maintainer  : Dinko Osrecki
-}
module Lecture7Exercises where

import           Data.Tuple

-- | 1
--   Define the functions using partial application of existing functions.
-- | 1.1 a
--   Define a function that takes first three elements from the list.
takeThree :: [a] -> [a]
takeThree = take 3

-- | 1.1 b
--   Define a function that drops first three elements from the list.
dropThree :: [a] -> [a]
dropThree = drop 3

-- | 1.1 c
--   Define a function that takes one element and repeats it 100 times
--   in a list.
hundredTimes :: a -> [a]
hundredTimes = replicate 100

-- | 1.2 a
--   Define a function that indexes the elements in a list.
index :: (Num b, Enum b) => [a] -> [(b,a)]
index = zip [0..]

-- | 1.2 b
--   Write the function that does the same, but with index at the 2nd position.
index' :: (Num b, Enum b) => [a] -> [(a,b)]
index' = zipWith swap' [0..]
  where swap' = curry swap

-- | 1.3
--   Define a function that returns a string of length 'n' consisting of
--   characters '='.
divider :: Int -> String
divider = replicate' '='
  where replicate' = flip replicate

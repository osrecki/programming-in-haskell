{-|
Module      : Homework3Tasks
Description : Solutions to homework 3 tasks
Maintainer  : Dinko Osrecki
-}
module Homework3Tasks where

import           Data.Char

-- | 1
--   A local maximum in a list is an element which is strictly
--   greater than both the elements immediately before and after
--   it. Define a function which finds all local maxima in the
--   list and returns them in order.
localMaxima :: [Int] -> [Int]
localMaxima (x:xs@(y:z:_))
  | y > x && y > z = y : localMaxima xs
  | otherwise      = localMaxima xs
localMaxima _ = []

-- | 2
--   Define a function that does the following transformation:
--   [(1, "AEIOULNRST"), (2, "DG"), ...] => [('a',1),('e',1),...,('d',2),...]
transform :: [(Int,String)] -> [(Char,Int)]
transform = concatMap transformOne

transformOne :: (Int,String) -> [(Char,Int)]
transformOne (n,s) = map charToTuple s
  where
    charToTuple c = (toLower c, n)

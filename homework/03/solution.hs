{-|
Module      : Homework3Tasks
Description : Solutions to homework 3 tasks
Maintainer  : Dinko Osrecki
-}
module Homework3Tasks where

import           Data.Bits
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

-- | 3
--   Implement a simplified version of the cellular automaton,
--   which works on a fixed-size array, and only implements
--   the Rule 90.
--   <https://en.wikipedia.org/wiki/Rule_90>
--
--   Example for Sierpinski triangle:
-- >  let fs = replicate 7 False
-- >  let xs = fs ++ [True] ++ fs
-- >  putStrLn $ pretty $ take 8 $ rule90 xs
--
rule90 :: [Bool] -> [[Bool]]
rule90 xs = xs : rule90 (rule90Step xs)

-- | Given current step, calculates the next one.
rule90Step :: [Bool] -> [Bool]
rule90Step xs = zipWith xor ps qs
  where
    ps = False : init xs
    qs = tail xs ++ [False]

pretty :: [[Bool]] -> String
pretty = unlines . map prettyOne

-- | Converts one step to String.
prettyOne :: [Bool] -> String
prettyOne = map boolToChar
  where
    boolToChar True  = '#'
    boolToChar False = ' '

-- | 4
--   Define a function that generate a look-and-say sequence.
--   <https://en.wikipedia.org/wiki/Look-and-say_sequence>
f :: [String]
f = "1" : map fStep f

-- | Given the current step, calculates the next one.
fStep :: String -> String
fStep "" = ""
fStep s@(c:_) = l ++ [c] ++ fStep rest
  where
    l = show $ length $ takeWhile (== c) s
    rest = dropWhile (== c) s

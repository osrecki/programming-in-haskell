{-|
Module      : Lecture4Exercises
Description : Solutions to Lecture 4 exercises
Maintainer  : Dinko Osrecki
-}
module Lecture4Exercises where

import           Data.Char
import           Data.List

-- EXERCISE 01 ----------------------------------------------------------------

{-
  1.1
  - Define a function which takes the head of the first list element.
    If the first element has no head, it takes the head of the second
    element. If the second element has no head, it takes the head of the
    third element. If none of this works, the function returns an error.
-}
headHunter :: [[a]] -> a
headHunter ((x:_):_)     = x
headHunter (_:(x:_):_)   = x
headHunter (_:_:(x:_):_) = x
headHunter _             = error "no such element"

{-
  1.2
  - Define a function which returns the first column of a matrix.
-}
firstColumn :: (Num a) => [[a]] -> [a]
firstColumn []          = []
firstColumn ([]:_)      = error "invalid matrix"
firstColumn ((x:_):xss) = x : firstColumn xss

{-
  1.3
  - Define a function which repeats three times the initial letter of each
    word in a string.
-}
shoutOutLoud :: String -> String
shoutOutLoud = unwords . map (repeatFirst 3) . words

repeatFirst :: Int -> String -> String
repeatFirst _ ""      = ""
repeatFirst 1 s       = s
repeatFirst n s@(x:_) = repeatFirst (n - 1) (x:s)

-- EXERCISE 02 ----------------------------------------------------------------

{-
  2.1
  - Define a function which pads the shorter of two strings with trailing
    spaces and returns both strings capitalized.
-}
pad :: String -> String -> (String, String)
pad a b = (transform a, transform b)
  where
    l = maxBy length a b
    transform s = appendSpaces (l - length s) $ capitalize s

maxBy :: Ord a => (b -> a) -> b -> b -> a
maxBy f x y = max (f x) (f y)

appendSpaces :: Int -> String -> String
appendSpaces n s = s ++ replicate n ' '

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs
capitalize _      = ""

{-
  2.2
  - Define a function which returns the quartiles (q1,q2,q3) of a given list.
-}
quartiles :: [Double] -> (Double, Double, Double)
quartiles xs = (f 0.25, f 0.5, f 0.75)
  where
    f q = quantile q xs

-- | Implements estimate type R-6 (<https://en.wikipedia.org/wiki/Quantile>).
quantile :: Double -> [Double] -> Double
quantile q xs
  | h < 1     = head ys
  | h >= n    = last ys
  | otherwise = xi + d * (xj - xi)
  where
    ys = sort xs
    n = genericLength xs
    h = q * (n + 1)
    h' = floor h
    d = h - fromIntegral h'
    xi = ys !! (h' - 1)
    xj = ys !! h'

-- EXERCISE 03 ----------------------------------------------------------------

{-
  3
  - Redo exercise 2 using 'let' instead of 'where'.
-}
pad' :: String -> String -> (String, String)
pad' a b =
  let l = maxBy length a b
      transform s = appendSpaces (l - length s) $ capitalize s
  in (transform a, transform b)


quartiles' :: [Double] -> (Double, Double, Double)
quartiles' xs = let f q = quantile q xs in (f 0.25, f 0.5, f 0.75)

-- EXERCISE 04 ----------------------------------------------------------------

{-
  4.1
  - Write a function which takes in a pair (a, b) and a list [c] and returns
    the following string:
    "The pair [contains two ones|contains one one|does not contain a single
    one] and the second element of the list is <x>"
-}
describe :: (Show a) => (Int, Int) -> [a] -> String
describe (x, y) zs = describePair ++ " and " ++ describeList
  where
    describePair = "The pair " ++ case (x, y) of
      (1, 1) -> "contains two ones"
      (p, q) | p == 1 || q == 1 -> "contains one one"
      _      -> "does not contain a single one"
    describeList = "the second element of the list is " ++ show secondElem
    secondElem = head . tail $ zs

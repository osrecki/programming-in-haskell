{-|
Module      : Lecture4Exercises
Description : Solutions to in-class exercises for Lecture 4
Maintainer  : Dinko Osrecki
-}
module Lecture4Exercises where

import           Data.Char
import           Data.List

-- | 1.1
--   Define a function that takes the head of the first list element.
--   If the first element has no head, it takes the head of the second
--   element. If the second element has no head, it takes the head of the
--   third element. If none of this works, the function returns an error.
headHunter :: [[a]] -> a
headHunter []        = error "no such element"
headHunter ([]:xss)  = headHunter xss
headHunter ((x:_):_) = x

-- | 1.2
--   Define a function that returns the first column of a matrix.
firstColumn :: (Num a) => [[a]] -> [a]
firstColumn []          = []
firstColumn ([]:_)      = error "invalid matrix"
firstColumn ((x:_):xss) = x : firstColumn xss

-- | 1.3
--   Define a function that repeats three times the initial letter of
--  each word in a string.
shoutOutLoud :: String -> String
shoutOutLoud = unwords . map (repeatFirst 3) . words

repeatFirst :: Int -> String -> String
repeatFirst _ ""      = ""
repeatFirst 1 s       = s
repeatFirst n s@(x:_) = repeatFirst (n - 1) (x:s)

-- | 2.1
--   Define a function that pads the shorter of two strings with
--   trailing spaces and returns both strings capitalized.
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

-- | 2.2
--   Define a function that returns the quartiles (q1,q2,q3) of a given list.
quartiles :: [Double] -> (Double, Double, Double)
quartiles xs = (f 0.25, f 0.5, f 0.75)
  where
    f q = quantile q xs

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

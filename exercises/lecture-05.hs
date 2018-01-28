{-|
Module      : Lecture5Exercises
Description : Solutions to in-class exercises for Lecture 5
Maintainer  : Dinko Osrecki
-}
module Lecture5Exercises where

-- | 1.1
--   Define a recursive function to compute the product of a list of elements.
product' :: (Num a) => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

-- tail recursive
product'' :: (Num a) => [a] -> a
product'' xs = f 1 xs
  where
    f acc (y:ys) = f (acc * y) ys
    f acc []     = acc

-- usign foldr
product''' :: (Num a) => [a] -> a
product''' xs = foldr (*) 1 xs

-- | 1.2
--   Define a recursive function that takes a list of lists and
--   returns a list of their heads.
headsOf :: [[a]] -> [a]
headsOf []          = []
headsOf ((x:_):xss) = x : headsOf xss
headsOf ([]:xss)    = headsOf xss

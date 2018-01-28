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

-- | 2.1
--   Define a recursive function 'modMult n m xs' that multiplies each
--   element of a list 'xs' with 'n' modulo 'm'.
modMult :: (Integral a, Num b) => a -> a -> [b] -> [b]
modMult _ _ [] = []
modMult n m (x:xs) = x' : modMult n m xs
  where
    x' = x * fromIntegral (n `mod` m)

-- | 2.2
--   Define a function that adds the value of the preceding element to
--   each element of the list. The first element gets no value added.
addPredecessor :: Num a => [a] -> [a]
addPredecessor xs = f (0:xs) []
  where
    f (x:y:ys) acc = f (y:ys) (acc ++ [x+y])
    f _        acc = acc

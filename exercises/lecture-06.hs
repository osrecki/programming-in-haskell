{-|
Module      : Lecture6Exercises
Description : Solutions to Lecture 6 exercises
Maintainer  : Dinko Osrecki
-}
module Lecture6Exercises where

-- EXERCISE 01 ----------------------------------------------------------------

{-
  1.1
  - Write an accumulator-style recursive definition of length.
-}
length' :: [a] -> Int
length' xs = len xs 0
  where
    len [] n     = n
    len (_:ys) n = len ys (n + 1)

{-
  1.2 a
  - Write an accumulator-style recursive definition of a function which
    returns the maximum element at the first position and the maximum element
    at the second position in a pair.
-}
maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "empty list"
maxUnzip ((x,y):zs) = maxUnzipRec zs (x,y)
  where
    maxUnzipRec [] (a,b)         = (a,b)
    maxUnzipRec ((p,q):rs) (a,b) = maxUnzipRec rs (max p a, max q b)

{-
  1.2 b
  - Write the same function without an accumulator.
-}
maxUnzip' :: [(Int, Int)] -> (Int, Int)
maxUnzip' []      = error "empty list"
maxUnzip' [(a,b)] = (a,b)
maxUnzip' ((a,b):xs) = (max a c, max b d)
  where
    (c, d) = maxUnzip' xs

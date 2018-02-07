{-# LANGUAGE LambdaCase #-}

{-|
Module      : Homework4Tasks
Description : Solutions to homework 4 tasks
Maintainer  : Dinko Osrecki
-}
module Homework4Tasks where

import           Data.Function
import           Data.List

-- | 1
--   Define following recursive functions and structures
--   with 'fix' and lambda functions only.

-- | 1 a
--   Define non-accumulator style factorial.
factorial :: (Num a, Eq a) => a -> a
factorial = fix (\rec n -> if n == 0 then 1 else n * rec (n - 1))

-- | 1 b
--   Define non-accumulator style sum.
sum' :: (Num a) => [a] -> a
sum' = fix (\rec -> \case { x:xs -> x + rec xs ; [] -> 0 })

-- | 1 c
--   Define accumulator style factorial.
factorial' :: (Num a, Eq a) => a -> a
factorial' = fix (\rec acc n -> if n == 0 then acc else rec (acc * n) (n - 1)) 1

-- | 1 d
--   Define accumulator style sum.
sum'' :: (Num a) => [a] -> a
sum'' = fix (\rec acc -> \case { x:xs -> rec (acc + x) xs ; _ -> acc }) 0

-- | 1 e
--   Define the list of natural numbers.
nats :: [Integer]
nats = fix (\rec n -> n : rec (n + 1)) 1

-- | 1 f
--   Define map.
map' :: (a -> b) -> [a] -> [b]
map' f = fix (\rec -> \case { y:ys -> f y : rec ys ; _ -> [] })

-- | 1 g
--   Define zip.
zip' :: [a] -> [b] -> [(a,b)]
zip' = fix (\rec xs ys -> case (xs,ys) of
                            (a:as,b:bs) -> (a,b) : rec as bs
                            _           -> [])

-- | 2 a
--   Write a function which generates a list of all k-sized
--   subsets of the given set. Set is represetented with a
--   list which may contain duplicate elements.
subsets :: (Eq a) => Int -> [a] -> [[a]]
subsets n xs = subsetsUniq n $ nub xs

-- for set (list) with unique elements
subsetsUniq :: (Eq a) => Int -> [a] -> [[a]]
subsetsUniq 0 _      = [[]]
subsetsUniq _ []     = []
subsetsUniq n (x:xs) = ys ++ zs
  where
    ys = map (x:) $ subsetsUniq (n-1) xs
    zs = subsetsUniq n xs

-- | 2 b
--   Write a function which generates a list of all partitions
--   of the given set. Each partition is represented as a list
--   of lists, the sublist being the disjoint subsets of the
--   given set.
partitions :: [a] -> [[[a]]]
partitions [] = error "empty set"
partitions xs = partitions' xs

partitions' :: [a] -> [[[a]]]
partitions' []     = [[]]
partitions' (x:xs) = concatMap (extendPartition x) (partitions' xs)

-- | Extends a given partition into several partitions by
--   adding the element 'x' in two following ways:
--    1. add a singleton subset [x]
--    2. add element x to each existing subset in turn
--
--   Example:
-- >>>  extendPartition 0 [[1],[2,3]]
-- [[[0,1],[2,3]], [[1],[0,2,3]], [[1],[2,3],[0]]]
extendPartition :: a -> [[a]] -> [[[a]]]
extendPartition x []       = [[[x]]]
extendPartition x (xs:xss) = ((x:xs):xss) : map (xs:) (extendPartition x xss)

{-# LANGUAGE LambdaCase #-}

{-|
Module      : Homework4Tasks
Description : Solutions to homework 4 tasks
Maintainer  : Dinko Osrecki
-}
module Homework4Tasks where

import           Data.Function

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

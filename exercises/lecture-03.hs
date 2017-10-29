{-|
Module      : Lecture3Exercises
Description : Solutions to in-class exercises for Lecture 3
Maintainer  : Dinko Osrecki
-}
module Lecture3Exercises where

import           Data.Char
import           Data.List

-- | 1. Without using the ':t' command, determine the types of the
--   following functions:
foo10 :: String -> [String]
foo10 w = [x ++ y | x <- lines w, y <- lines w]

foo11 :: String -> [(String, String)]
foo11 w = [(x,y) | x <- lines w, y <- lines w]

foo12 :: String -> [String]
foo12 w = [y : x | x <- lines w, y <- w]

foo13 :: String -> [(String, String)]
foo13 w = [(y:x, w) | x <- lines w, y <- w]

foo14 :: String -> [(Char, Bool)]
foo14 w = [(x, x=='a') | x <- w ]

foo15 :: String -> String
foo15 s = tail [ c | c <- s, isLower c ]

foo16 :: String -> [(Char, Char)]
foo16 s = zip [ c | c <- s, isLower c ] "Haskell"

foo17 :: Int -> Char -> String
foo17 n c = reverse $ drop n $ c : "Haskell"

foo18 :: String -> String
foo18 xs = last $ words xs

foo19 :: Char -> String -> String
foo19 x z = x : 'y' : z

-- | 2. Without using the ':t' command, determine the types of the
--   following functions:
foo20 :: [a] -> [a]
foo20 xs = tail xs ++ [head xs]

foo21 :: [a] -> (a, [a])
foo21 xs = (head xs, tail xs)

foo22 :: a -> [a] -> [a]
foo22 x xs = x:xs

foo23 :: [a] -> [a]
foo23 l = init $ tail l

foo24 :: [[a]] -> [a] -> [a]
foo24 xss ys = concat xss ++ ys

foo25 :: [[a]] -> [a] -> (a, a)
foo25 xss ys = (head $ concat xss, head ys)

foo26 :: [[[a]]] -> a
foo26 xs = head $ concat $ concat xs

foo27 :: [a] -> [[a]]
foo27 cs = [[c1,c2] | c1 <- cs, c2 <- cs]

foo28 :: [[a]] -> [[a]]
foo28 cs = [concat [c1,c2] | c1 <- cs, c2 <- cs]

foo29 :: [a] -> [a]
foo29 cs = concat [[c1,c2] | c1 <- cs, c2 <- cs]

-- | 3. Without using the ':t' command, determine the types of the
--   following functions:
foo30 :: (Eq a) => a -> [a] -> a
foo30 x ys = if x == head ys then x else last ys

foo31 :: (Ord a) => a -> [a] -> a
foo31 x ys = if x < head ys then x else last ys

foo32 :: (Eq a) => [a] -> [[a]] -> a
foo32 xs yss = if xs == head yss then head xs else last xs

foo33 :: (Num b, Enum b) => Bool -> [a] -> [(b, a)]
foo33 x ys = if x then zip [1..9] ys else []

foo34 :: (Num a, Enum a) => String -> [(a, String)]
foo34 w = zip [0..] (lines w)

foo35 :: (Integral a, Fractional a) => a -> a -> a
foo35 x y = if odd x then y else x / 10

foo36 :: (Eq a, Ord a) => [a] -> Bool
foo36 xs = sort xs == xs

foo37 :: (Show a) => a -> [[a]] -> String
foo37 x xs = show x ++ (show $ concat xs)

foo38 :: (Num a) => [[a]] -> a
foo38 xs = sum $ concat xs

foo39 :: (Num a, Ord a) => [a] -> [[a]] -> a
foo39 xs yss = sum $ [min x y | x <- xs, ys <- yss, y <- ys]

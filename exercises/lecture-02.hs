{-|
Module      : Lecture2Exercises
Description : Solutions to Lecture 2 exercises
Maintainer  : Dinko Osrecki
-}
module Lecture2Exercises where

import           Data.Char
import           Data.List

-- EXERCISE 01 ----------------------------------------------------------------

{-
  1.1
  - Define a function which returns a list without the first three and the
    last three elements.
-}
noFirst3NoLast3 :: [a] -> [a]
noFirst3NoLast3 xs = zipWith (curry fst) (drop 3 xs) (drop 6 xs)

{-
  1.2
  - Define a function which takes a person's name and a surname and returns
    a string consisting of person's initials.
-}
initials :: String -> String -> String
initials (n:_) (s:_) = [n, '.', ' ', s, '.']
initials _ _         = error "Invalid parameters."

{-
  1.3
  - Define a function which concatenates two strings, so that the longest
    string always comes first.
-}
concatenate :: [a] -> [a] -> [a]
concatenate a b = s ++ f
  where
    [f, s] = sortBy compareLength [a, b]

compareLength :: [a] -> [b] -> Ordering
compareLength [] []         = EQ
compareLength (_:_) []      = GT
compareLength [] (_:_)      = LT
compareLength (_:xs) (_:ys) = compareLength xs ys

{-
  1.4
  - Define a function which returns an empty list if input is an empty list,
    otherwise it returns its first element wrapped inside a singleton list.
-}
safeHead :: [a] -> [a]
safeHead []    = []
safeHead (x:_) = [x]

{-
  1.5
  - Define a function which checks whether a list contains duplicate elements
    (use 'nub').
-}
hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates xs = compareLength xs (nub xs) /= EQ

-- EXERCISE 02 ----------------------------------------------------------------

{-
  2.1
  - Define a function which generates a sequence [a*2, (a+1)*2, ..., b*2] and
    also works when b < a.
-}
doublesFromTo :: (Integral a) => a -> a -> [a]
doublesFromTo a b
  | a <= b    = [2 * x | x <- [a..b]]
  | otherwise = [2 * x | x <- [a,a-1..b]]

{-
  2.2
  - Redefine 'ceasarCode n xs' so that it shifts all letters a specified
    number of positions 'n', converts all input to lowercase, and ensures
    that letters remain within the ['a'..'z'] interval.
-}
ceasarCode :: Int -> String -> String
ceasarCode n = map (shiftChar n . toLower)

shiftChar :: Int -> Char -> Char
shiftChar 0 c = c
shiftChar n c = shiftChar (n - 1) (next c)
  where
    next 'z' = 'a'
    next a   = succ a

-- EXERCISE 03 ----------------------------------------------------------------

{-
  3.1
  - Define a function which computes the total number of letters in a string,
    thereby ignoring the whitespaces and all words shorter than three letters.
-}
letterCount :: String -> Int
letterCount xs = sum [length ys | ys <- words xs, length ys > 2]

{-
  3.2
  - Redefine 'isPalindrome' so that it is case insensitive and works correctly
    for strings that contain whitespaces.
-}
isPalindrome :: String -> Bool
isPalindrome xs = s == reverse s
  where
    s = (map toLower . filter (/= ' ')) xs

{-
  3.3
  - Define a function which takes a list of lists, reverts each individual
    list, and concatenates all of them, but in the reverse order.
-}
reverseConcat :: [[a]] -> [a]
reverseConcat = concat . reverse . map reverse

-- EXERCISE 04 ----------------------------------------------------------------

{-
  4.1.1
  - Define a function which returns the coordinates of all points within the
    ([-10..10],[-10..10]) interval that fall inside a circle of radius 'r'
    with center '(x,y)'.
-}
inCircleXs :: (Floating a, Ord a, Enum a) => Circle a -> [Point a]
inCircleXs c = [(a, b) | a <- [-10..10], b <- [-10..10], inCircle c (a, b)]

{-
  4.1.2
  - Redefine the function so that it takes the resolution of the grid as an
    additional argument.
-}
inCircleXs' :: (Floating a, Ord a, Enum a) => Circle a -> a -> [Point a]
inCircleXs' c s = [(a, b) | a <- ds, b <- ds, inCircle c (a, b)]
  where
    ds = map (* s) [-10/s..10/s]

type Point a = (a, a)
type Circle a = (Point a, a)

inCircle :: (Floating a, Ord a) => Circle a -> Point a -> Bool
inCircle (o, r) p = distance o p <= r

distance :: (Floating a) => Point a -> Point a -> a
distance (x1, y1) (x2, y2) = sqrt (dx * dx + dy * dy)
  where
    dx = x1 - x2
    dy = y1 - y2

{-
  4.2
  - Define a function which, given a list xs = [x1,x2,...], generates the
    pairs [(x1,x2),(x2,x3),...].
-}
steps :: [a] -> [(a, a)]
steps [] = []
steps xs = zip xs $ tail xs

-- EXERCISE 05 ----------------------------------------------------------------

{-
  5.1
  - Define a function which returns the indices of element 'x' in list 'xs'
    (if 'x' appears multiple times, there will be a number of such indices).
-}
indices :: (Eq a) => a -> [a] -> [Int]
indices x xs = [i | (i, y) <- ys, y == x]
  where
    ys = zip [0..] xs

{-
  5.2
  - Define a function which prefixes all lines from string 's' with a line
    number.
-}
showLineNumbers :: String -> String
showLineNumbers = unlines . zipWith prepend numbers . lines
  where
    prepend i x = show i ++ " " ++ x
    numbers     = [1..]

{-
  5.3.1
  - Define a function which returns 'True' if 'xs' and 'ys' have any identical
    elements that are aligned (appear at the same position in both lists).
-}
haveAlignment :: (Eq a) => [a] -> [a] -> Bool
haveAlignment _ [] = False
haveAlignment [] _ = False
haveAlignment (x:xs) (y:ys)
  | x == y    = True
  | otherwise = haveAlignment xs ys

{-
  5.3.2
  - Define a function which returns the aligned subsequences.
-}
common :: (Eq a) => [a] -> [a] -> [a]
common _ [] = []
common [] _ = []
common (x:xs) (y:ys)
  | x == y    = x : common xs ys
  | otherwise = common xs ys

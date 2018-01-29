{-|
Module      : Homework2Tasks
Description : Solutions to homework 2 tasks
Maintainer  : Dinko Osrecki
-}
module Homework2Tasks where

-- | 1
--   Given a DNA strand, return its RNA complement.
toRNA :: String -> String
toRNA = map toRNAChar

toRNAChar :: Char -> Char
toRNAChar 'G' = 'C'
toRNAChar 'C' = 'G'
toRNAChar 'T' = 'A'
toRNAChar 'A' = 'U'
toRNAChar _   = error "invalid DNA nucletiode"

-- | 2
--   Define following functions using recursion.
-- | 2 a
--   Define a multiplication function in terms of addition.
multiply :: Int -> Int -> Int
multiply a b = sign v
  where
    sign = negIf (signum a /= signum b)
    v = mult (abs a) (abs b)

-- for b >= 0
mult :: Int -> Int -> Int
mult _ 0 = 0
mult a 1 = a
mult a b = a + mult a (b - 1)

-- | 2 b
--   Define a division function in terms of subtraction.
divide :: Int -> Int -> Int
divide a b = sign v
  where
    sign = negIf (signum a /= signum b)
    v = div' (abs a) (abs b)

-- for a >= 0 && b > 0
div' :: Int -> Int -> Int
div' _ 0 = error "division by zero"
div' a b
  | a < b     = 0
  | otherwise = 1 + div' (a - b) b

negIf :: Bool -> Int -> Int
negIf True x  = - abs x
negIf False x = abs x

-- | 2 c
--   Define a function to find the greatest common divisor.
greatestCD :: Int -> Int -> Int
greatestCD x 0 = x
greatestCD x y = greatestCD y (x `mod` y)

-- | 3
--   Implement a function which converts the number into its
--   English counterpart. It must support numbers up to millions.
numberToWords :: Int -> String
numberToWords n = unwords $ reverse $ zipWith (++) words' suffixes
  where
    groups = numToGroups n
    words' = map (groupToString "") groups

-- | Breaks a number into 3-digit groups.
numToGroups :: (Integral a) => a -> [a]
numToGroups n
    | d == 0    = [n]
    | otherwise = m : numToGroups d
  where
    (d,m) = n `divMod` 1000

-- | Converts a 3-digit group to word.
groupToString :: String -> Int -> String
groupToString prefix n
    | n == 0    = ""
    | n < 10    = prefix ++ ones n
    | n < 20    = prefix ++ teens n
    | n < 100   = prefix ++ tens d ++ groupToString "-" m
    | n < 1000  = ones d' ++ " hundred " ++ groupToString "and " m'
    | otherwise = "invalid number"
  where
    (d,m)  = n `divMod` 10
    (d',m') = n `divMod` 100

ones :: Int -> String
ones n = xs !! (n - 1)
  where
    xs = ["one", "two", "three", "four", "five", "six", "seven", "eight",
          "nine"]

teens :: Int -> String
teens n = xs !! (n - 10)
  where
    xs = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
          "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: Int -> String
tens n = xs !! (n - 2)
  where
    xs = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
          "ninety"]

suffixes :: [String]
suffixes = ["", " thousand", " million", " billion", " thrillion"]

-- | 4
--   Define your own version of undefined.
--
--   undefined has an unconstrained type 'a' which allows us to
--   use it anywhere in the code. This is because its type gets
--   specialized based on the context in which it is used.
--
--   E.g., undefined specialized to type Int:
-- >  f :: Int -> Int
-- >  f x = x + undefined
--
--   undefined does not have any meaningful value. This means that
--   program breaks if it reaches a step where the value of undefined
--   is needed.
--
--   However, it comes in handy when we need a placeholder in the
--   code. It can be implemented in several ways:
-- as an infinite loop
undefined' :: a
undefined' = undefined'

-- as a crash
undefined'' :: a
undefined'' | False = undefined'' -- non-exhaustive pattern matching

-- example using custom undefined
funcWithUndefined :: Int -> Int
funcWithUndefined x = x + undefined'

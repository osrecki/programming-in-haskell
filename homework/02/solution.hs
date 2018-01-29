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

undefinedFunc :: Int -> Int
undefinedFunc x = x + undefined

-- as an infinite loop
undefined' :: a
undefined' = undefined'

-- as a crash
undefined'' :: a
undefined'' | False = undefined''

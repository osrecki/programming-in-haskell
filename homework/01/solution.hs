{-|
Module      : Homework1Tasks
Description : Solutions to homework 1 tasks
Maintainer  : Dinko Osrecki
-}
module Homework1Tasks where

-- | 1.1
--   Write a function that checks if a year is a leap year.
isLeapYear :: Int -> Bool
isLeapYear y = divisibleBy 4 && (not(divisibleBy 100) || divisibleBy 400)
  where divisibleBy d = y `rem` d == 0

-- | 1.2
--   Generate a list of all leap years between 1996 and 2017.
leapList :: [Int]
leapList = filter isLeapYear [1996..2017]

-- | 2. Implement the exponential function
-- | 2. a)
--   Write a function which evaluates polynomial of single indeterminante
--   'x' where polynomial coefficients are represented by a list of Doubles.
evaluate :: Double -> [Double] -> Double
evaluate x as = sum $ zipWith term ks as
  where
    term k a = a * (x ** k)
    ks       = [0..]

-- | 2. b)
--   Define a function that calculates the nth factorial.
factorial :: Double -> Double
factorial n
  | n >= 0    = product [1..n]
  | otherwise = error "argument must be non-negative"

-- | 2. c)
--   Define an infinite list of coefficients for Maclaurin series.
maclaurin :: [Double]
maclaurin = map (\n -> 1 / factorial n) [0..]

-- | 2. d)
--   Combine evaluate, factorial and maclaurin to create a function
--   which approximates exp from the first 170 terms of the series.
exp' :: Double -> Double
exp' n = evaluate n $ take 170 maclaurin

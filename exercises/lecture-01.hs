{-|
Module      : Lecture1Exercises
Description : Solutions to Lecture 1 exercises
Maintainer  : Dinko Osrecki
-}
module Lecture1Exercises where

-- EXERCISE 01 ----------------------------------------------------------------

{-
  1.1
  - Define function that concatenates three strings, but drops the middle one
    if it's shorter than 2 characters (use 'length' function).
-}
concat3 :: String -> String -> String -> String
concat3 a b c
  | length b < 2 = a ++ c
  | otherwise    = a ++ b ++ c

{-
  1.2.1
  - Give a simpler definition of 'showSalary', using only one if-then-else
    construct.
-}
showSalary :: (Num a, Eq a, Show a) => a -> a -> String
showSalary amount bonus = salaryStr ++ bonusStr
  where
    salaryStr = "Salary is " ++ show amount
    bonusStr  =
      if bonus /= 0
      then ", and bonus is " ++ show bonus ++ "."
      else ""

{-
  1.2.2
  - Additionally check that salary is non-negative. If it is negative, return
    an adequate message.
-}
showSalary' :: (Num a, Ord a, Show a) => a -> a -> String
showSalary' amount bonus
  | amount < 0 = error "Salary must be positive."
  | otherwise  = showSalary amount bonus

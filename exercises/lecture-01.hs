{-|
Module      : Lecture1Exercises
Description : Solutions to in-class exercises for Lecture 1
Maintainer  : dinko.osrecki@gmail.com
-}
module Lecture1Exercises where

concat3 :: String -> String -> String -> String
concat3 a b c
  | length b < 2 = a ++ c
  | otherwise    = a ++ b ++ c

showSalary :: (Num a, Eq a, Show a) => a -> a -> String
showSalary amount bonus = salaryStr ++ bonusStr
  where
    salaryStr = "Salary is " ++ show amount
    bonusStr  =
      if bonus /= 0
      then ", and bonus is " ++ show bonus ++ "."
      else ""

showSalary' :: (Num a, Ord a, Show a) => a -> a -> String
showSalary' amount bonus
  | amount < 0 = error "Salary must be positive."
  | otherwise  = showSalary amount bonus

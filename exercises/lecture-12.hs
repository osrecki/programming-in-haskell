{-|
Module      : Lecture12Exercises
Description : Solutions to Lecture 12 exercises
Maintainer  : Dinko Osrecki
-}
module Lecture12Exercises where

-- EXERCISE 01 ----------------------------------------------------------------

{-
  1.1
  - Define a 'main' function which reads in two strings and prints them out
    concatenated and reversed.
-}
main :: IO ()
main = do
  ls <- sequence [getLine, getLine]
  putStrLn . reverse . concat $ ls

{-
  1.2
  - Write a function 'threeNumbers' which reads in three numbers and prints out
    their sum.
-}
threeNumbers :: IO ()
threeNumbers = do
  ls <- mapM (fmap readInt) [getLine, getLine, getLine]
  print . sum $ ls

readInt :: String -> Int
readInt = read

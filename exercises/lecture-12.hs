{-|
Module      : Lecture12Exercises
Description : Solutions to Lecture 12 exercises
Maintainer  : Dinko Osrecki
-}
module Lecture12Exercises where

import           Text.Read (readMaybe)

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

-- EXERCISE 02 ----------------------------------------------------------------

{-
  2.1
  - Define a function which reads in three strings and outputs them to the
    screen as one string, while it returns its total length.
-}
threeStrings :: IO Int
threeStrings = do
  l <- concat <$> sequence [getLine, getLine, getLine]
  putStrLn l
  return $ length l

{-
  2.2
  - Define a function which reads in a number and returns that number converted
    into an 'Int'. Input should be repeated until the user enters a number (a
    string containing only digits).
-}
askNumber9 :: IO Int
askNumber9 = do
  n <- readMaybeInt <$> getLine
  maybe askNumber9 return n

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

{-
  2.3 a
  - Define a function 'askUser m p' which returns an action that prints out 'm',
    reads in a string from the input, repeats the input until the input string
    satisfies the function 'p', and then returns the input string.
-}
askUser :: String -> (String -> Bool) -> IO String
askUser m p = do
  putStrLn m
  readUntil p

readUntil :: (String -> Bool) -> IO String
readUntil p = do
  line <- getLine
  if p line
    then return line
    else readUntil p

{-
  2.3 b
  - Generalize the function to:
    askUser' :: Read a => String -> (String -> Bool) -> IO a
-}
askUser' :: Read a => String -> (String -> Bool) -> IO a
askUser' m p = do
  putStrLn m
  readUntil' p

readUntil' :: (Read a) => (String -> Bool) -> IO a
readUntil' p = do
  line <- getLine
  if p line
    then return $ read line
    else readUntil' p

{-
  2.4
  - Define a function which reads in strings until the user inputs an empty
    string, and then returns a list of strings received as input.
-}
inputStrings :: IO [String]
inputStrings = takeWhile (not . null) <$> getLines

getLines :: IO [String]
getLines = lines <$> getContents

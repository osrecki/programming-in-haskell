{-|
Module      : Lecture12Exercises
Description : Solutions to Lecture 12 exercises
Maintainer  : Dinko Osrecki
-}
module Lecture12Exercises where

import           Control.DeepSeq
import           Control.Monad   (replicateM, void)
import qualified Data.Set        as Set
import           System.IO
import           Text.Read       (readMaybe)

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
  l <- concat <$> replicateM 3 getLine
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

-- EXERCISE 03 ----------------------------------------------------------------

{-
  3.1
  - Define a function which reads in a number, then reads in that many
    strings, and finally prints these strings in reverse order.
-}
reverseStrings :: IO ()
reverseStrings = do
  n <- read <$> getLine
  xs <- reverse <$> replicateM n getLine
  mapM_ putStrLn xs

{-
  3.2
  - Give recursive definitions for 'sequence' and 'sequence_'.
-}
sequence' :: (Monad m) => [m a] -> m [a]
sequence' []     = return []
sequence' (m:ms) = (:) <$> m <*> sequence' ms

sequence_' :: (Monad m) => [m a] -> m ()
sequence_' = void . sequence'

-- using foldr
sequence'' :: (Monad m) => [m a] -> m [a]
sequence'' = foldr (\m acc -> (:) <$> m <*> acc) (return [])

{-
  3.3
  - Give recursive definitions for 'mapM' and 'mapM_'.
-}
mapM' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM' _ []     = return []
mapM' f (m:ms) = (:) <$> f m <*> mapM' f ms

mapM_' :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_' f xs = void $ mapM' f xs

-- using sequence'
mapM'' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM'' f = sequence' . map f

{-
  3.4
  - Define a function which prints out the Pythagorean triples whose all
    sides are <= 100. Every triple should be in a separate line.
-}
printTriples :: IO ()
printTriples = mapM_ print $ triples 100

triples :: Int -> [(Int, Int, Int)]
triples n =
  [(x, y, z) | x <- [1..n], y <- [x+1..n], z <- [y+1..n], z*z == x*x + y*y]

-- EXERCISE 04 ----------------------------------------------------------------

{-
  4.1
  - Define a function which removes every second line from standard input and
    prints the result to standard output.
-}
filterOdd :: IO ()
filterOdd = interact
  $ unlines . map snd . filter (odd . fst) . enumerate . lines

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

{-
  4.2
  - Define a function which prefixes each line from standard input with a line
    number (number + space).
-}
numberLines :: IO ()
numberLines = interact $ unlines . map prependNumber . enumerate . lines
  where
    prependNumber (n, l) = show n ++ " " ++ l

{- 4.3
  - Define a function to remove all words from standard input which are
    contained in the given set of words.
-}
filterWords :: Set.Set String -> IO ()
filterWords ws = interact $ unwords . filter (`Set.notMember` ws) . words

-- EXERCISE 05 ----------------------------------------------------------------

{-
  5.1
  - Define a function which counts the number of characters, words, and lines
    in a file.
-}
wc :: FilePath -> IO (Int, Int, Int)
wc f = withFile f ReadMode $ \h -> do
  s <- hGetContents h
  return $!! charsWordsLines s  -- 'deepseq' to read file eagerly

charsWordsLines :: String -> (Int, Int, Int)
charsWordsLines s = (c, w, l)
  where
    c = length s
    w = length $ words s
    l = length $ lines s

{-
  5.2
  - Define a function which copies the given lines of the first file into the
    second one.
-}
copyLines :: [Int] -> FilePath -> FilePath -> IO ()
copyLines xs f1 f2 = do
  s <- readFile f1
  writeFile f2
    $ unlines . map snd . filter ((`elem` xs) . fst) . enumerate $ lines s

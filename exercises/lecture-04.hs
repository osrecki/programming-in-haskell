{-|
Module      : Lecture4Exercises
Description : Solutions to in-class exercises for Lecture 4
Maintainer  : Dinko Osrecki
-}
module Lecture4Exercises where

-- | 1.1
--   Define a function that takes the head of the first list element.
--   If the first element has no head, it takes the head of the second
--   element. If the second element has no head, it takes the head of the
--   third element. If none of this works, the function returns an error.
headHunter :: [[a]] -> a
headHunter []        = error "no such element"
headHunter ([]:xss)  = headHunter xss
headHunter ((x:_):_) = x

-- | 1.2
--   Define a function that returns the first column of a matrix.
firstColumn :: (Num a) => [[a]] -> [a]
firstColumn []          = []
firstColumn ([]:_)      = error "invalid matrix"
firstColumn ((x:_):xss) = x : firstColumn xss

-- | 1.3
--   Define a function that repeats three times the initial letter of
--  each word in a string.
shoutOutLoud :: String -> String
shoutOutLoud = unwords . map (repeatFirst 3) . words

repeatFirst :: Int -> String -> String
repeatFirst _ ""      = ""
repeatFirst 1 s       = s
repeatFirst n s@(x:_) = repeatFirst (n - 1) (x:s)

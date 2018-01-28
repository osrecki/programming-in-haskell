{-|
Module      : Lecture5Exercises
Description : Solutions to in-class exercises for Lecture 5
Maintainer  : Dinko Osrecki
-}
module Lecture5Exercises where

-- | 1.1
--   Define a recursive function to compute the product of a list of elements.
product' :: (Num a) => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

-- tail recursive
product'' :: (Num a) => [a] -> a
product'' xs = f 1 xs
  where
    f acc (y:ys) = f (acc * y) ys
    f acc []     = acc

-- usign foldr
product''' :: (Num a) => [a] -> a
product''' xs = foldr (*) 1 xs

-- | 1.2
--   Define a recursive function that takes a list of lists and
--   returns a list of their heads.
headsOf :: [[a]] -> [a]
headsOf []          = []
headsOf ((x:_):xss) = x : headsOf xss
headsOf ([]:xss)    = headsOf xss

-- | 2.1
--   Define a recursive function 'modMult n m xs' that multiplies each
--   element of a list 'xs' with 'n' modulo 'm'.
modMult :: (Integral a, Num b) => a -> a -> [b] -> [b]
modMult _ _ [] = []
modMult n m (x:xs) = x' : modMult n m xs
  where
    x' = x * fromIntegral (n `mod` m)

-- | 2.2
--   Define a function that adds the value of the preceding element to
--   each element of the list. The first element gets no value added.
addPredecessor :: Num a => [a] -> [a]
addPredecessor xs = f (0:xs) []
  where
    f (x:y:ys) acc = f (y:ys) (acc ++ [x+y])
    f _        acc = acc

-- | 3.1
--   Define a function that given a list of triplets (x,y,z), filters all
--   triplets for which x==y==z.
equalTriplets :: (Eq a) => [(a, a, a)] -> [(a, a, a)]
equalTriplets [] = []
equalTriplets ((x,y,z):xs)
  | x == y && x == z = (x,y,z) : equalTriplets xs
  | otherwise        = equalTriplets xs

-- list comprehension
equalTriplets' :: (Eq a) => [(a, a, a)] -> [(a, a, a)]
equalTriplets' xs = [(x,y,z) | (x,y,z) <- xs, x == y, x ==z]

-- filter
equalTriplets'' :: (Eq a) => [(a, a, a)] -> [(a, a, a)]
equalTriplets'' xs = filter allEq xs
  where
    allEq (x,y,z) = x == y && x == z

-- | 3.2
--   Define your own version of the replicate function.
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

-- tail recursive
replicate'' :: Int -> a -> [a]
replicate'' n x = f n []
  where
    f 0 acc = acc
    f m acc = f (m - 1) (x:acc)

-- | 4
--   Extend 'take' so that, if n > length xs, the last element
--   of the list gets repeated.
take'' :: Int -> [a] -> [a]
take'' n xs
  | n > length xs = take n $ xs ++ repeat (last xs)
  | otherwise     = take n xs

-- | 4.1 a
--   Define your own recursive version of the drop function.
drop' :: Int -> [a] -> [a]
drop' n xs     | n <= 0 = xs
drop' _ []     = []
drop' n (_:xs) = drop' (n - 1) xs

-- | 4.1 b
--   Define drop'' (a wrapper function) so that for n < 0 the
--   function drops the elements from the end of the list.
drop'' :: Int -> [a] -> [a]
drop'' n xs
  | n < 0     = reverse $ drop' (abs n) (reverse xs)
  | otherwise = drop' n xs

-- | 4.2
--   Define a recursive function 'takeFromTo n1 n2 xs'.
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo n m xs
  | n < 0 = takeFromTo 0 m xs
  | m < n = []
takeFromTo _ _ []     = []
takeFromTo 0 0 (x:_)  = [x]
takeFromTo 0 j (x:xs) = x : takeFromTo 0 (j - 1) xs
takeFromTo i j (_:xs) = takeFromTo (i - 1) (j - 1) xs

-- | 5.1
--   Define a recursive function that retains every third element in a list.
eachThird :: [a] -> [a]
eachThird (_:_:x:xs) = x : eachThird xs
eachThird _          = []

-- alternative, more general solution
eachThird' :: [a] -> [a]
eachThird' = eachNth 3

eachNth :: Int -> [a] -> [a]
eachNth n = eachNthRec n n

eachNthRec :: Int -> Int -> [a] -> [a]
eachNthRec _ _ []     = []
eachNthRec n 1 (x:xs) = x : eachNthRec n n xs
eachNthRec n i (_:xs) = eachNthRec n (i - 1) xs

-- | 5.2
--   Define a recursive function that zips two lists in a crossing manner.
crossZip :: [a] -> [b] -> [(a, b)]
crossZip (x:x':xs) (y:y':ys) = (x,y') : (x',y) : crossZip xs ys
crossZip _ _                 = []

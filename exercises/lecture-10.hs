{-|
Module      : Lecture10Exercises
Description : Solutions to in-class exercises for Lecture 10
Maintainer  : Dinko Osrecki
-}
module Lecture10Exercises where

import           Control.Applicative ()
import           Data.List
import           Data.Maybe

-- | 1
data Sex = Male | Female deriving (Show, Read, Eq, Ord)
data Person = Person { personId  :: String
                     , firstName :: String
                     , lastName  :: String
                     , sex       :: Sex
                     , mother    :: Maybe Person
                     , father    :: Maybe Person
                     , partner   :: Maybe Person
                     , children  :: [Person]
                     } deriving (Show, Read, Ord)

instance Eq Person where
  p1 == p2 = personId p1 == personId p2

ann :: Person
ann = Person "001" "Ann" "Doe" Female Nothing Nothing Nothing [john]
john :: Person
john = Person "010" "John" "Doe" Male (Just ann) Nothing (Just jane) [bobby,lea]
jane :: Person
jane =  Person "020" "Jane" "Doe" Female Nothing Nothing (Just john) [bobby,lea]
bobby :: Person
bobby =  Person "100" "Bobby" "Doe" Male (Just jane) (Just john) Nothing []
lea :: Person
lea =  Person "200" "Lea" "Doe" Female (Just jane) (Just john) Nothing []

-- | 1.1
--   Define a function which returns mother of person's partner.
partnersMother :: Person -> Maybe Person
partnersMother p = partner p >>= mother

-- | 1.2
--   Define a function which checks whether the given person is
--   one of the children of its parents.
parentCheck :: Person -> Bool
parentCheck p = elem p $ parentsChildren p

parentsChildren :: Person -> [Person]
parentsChildren = nub . concatMap children . parents

parents :: Person -> [Person]
parents p = mapMaybe ($ p) [mother, father]

-- | 1.3
--   Define a function which returns the sister of the person,
--   it if exists.
sister :: Person -> Maybe Person
sister p = find ((== Female) . sex) . filter (/= p) . parentsChildren $ p

-- | 1.4
--   Define a function which returns all descendants of a person.
descendants :: Person -> [Person]
descendants p = kids ++ concatMap descendants kids
  where
    kids = children p

-- | 2
infixr 5 :-:
data MyList a = Empty | a :-: (MyList a) deriving (Show, Read, Ord, Eq)

-- | 2.1
--   Define a function to return the head of MyList.
listHead :: MyList a -> Maybe a
listHead Empty     = Nothing
listHead (x :-: _) = Just x

-- | 2.2
--   Define a function that works like 'map' but on a 'MyList'.
listMap :: (a -> b) -> MyList a -> MyList b
listMap _ Empty      = Empty
listMap f (x :-: xs) = f x :-: listMap f xs

-- | 3
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- | 3.1
--   Define a function which finds the maximum element in a tree.
--   Return an error if the tree is empty.
treeMax :: (Ord a) => Tree a -> a
treeMax Null = error "empty tree"
treeMax t    = maximum $ elems t

elems :: Tree a -> [a]
elems Null         = []
elems (Node x l r) = elems l ++ [x] ++ elems r

-- | 3.2
--   Define a function which will collect all elements from the
--   inner nodes of a tree into the list in the in-order traversal.
treeToList :: Tree a -> [a]
treeToList = elems

-- | 3.3
--   Define a function to prune the tree at a given level (root
--   has level 0).
levelCut :: Int -> Tree a -> Tree a
levelCut n _ | n < 0    = error "negative level"
levelCut _ Null         = Null
levelCut 0 (Node x _ _) = Node x Null Null
levelCut n (Node x l r) = Node x (nextLevelCut l) (nextLevelCut r)
  where
    nextLevelCut = levelCut (n - 1)
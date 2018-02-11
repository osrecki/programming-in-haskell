{-|
Module      : Lecture11Exercises
Description : Solutions to in-class exercises for Lecture 11
Maintainer  : Dinko Osrecki
-}
module Lecture11Exercises where

class Ageing a where
  maxAge :: a -> Int
  currentAge :: a -> Int
  makeOlder :: a -> a

-- | Person data type
data Sex = Male | Female deriving (Show, Read, Eq, Ord)
data Person = Person { personId  :: String
                     , firstName :: String
                     , lastName  :: String
                     , sex       :: Sex
                     , age       :: Int
                     } deriving (Read, Ord, Eq, Show)

instance Ageing Person where
  maxAge = const 123
  currentAge = age
  makeOlder p = p { age = age p + 1 }

-- | Dog data type
data Breed = Beagle | Husky | Pekingese deriving (Eq, Ord, Show, Read)
data Dog = Dog { dogName  :: String
               , dogBreed :: Breed
               , dogAge   :: Int
               } deriving (Eq, Ord, Show, Read)

instance Ageing Dog where
  maxAge d | dogBreed d == Husky = 29
  maxAge _ = 20

  currentAge = dogAge
  makeOlder d = d { dogAge = dogAge d + 1 }

toto :: Dog
toto = Dog "Toto" Beagle 16

ines :: Person
ines = Person "2692" "Ines" "Seni" Female 16

-- | 1.1
--   Define a function which compares ages relative to the
--   maximum age, so that, e.g. a 10-year old dog is considered
--   older than a 20-year old human.
compareRelativeAge :: (Ageing a, Ageing b) => a -> b -> Ordering
compareRelativeAge a b = compare (relativeAge a) (relativeAge b)

relativeAge :: (Ageing a) => a -> Float
relativeAge a = currentAge' a / maxAge' a
  where
    currentAge' = fromIntegral . currentAge
    maxAge' = fromIntegral . maxAge

-- | 1.2
--   Define a class 'Nameable' with function 'name'. Define
--   'Person' and 'Dog' as instances of this class. For a
--   person return "<Name> <Surname>", and for a dog return
--   "<Name> the Dog".
class Nameable a where
  name :: a -> String

instance Nameable Person where
  name p = firstName p ++ " " ++ lastName p

instance Nameable Dog where
  name d = dogName d ++ " the Dog"

-- | 2
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show, Eq)

elems :: Tree a -> [a]
elems Null         = []
elems (Node x l r) = elems l ++ [x] ++ elems r

-- | 2.1 a
--   Define a 'Takeable' class with a function 'takeSome'.
class Takeable t where
  takeSome :: Int -> t a -> [a]

-- | 2.1 b
--   Define '[]' and 'Tree' as instances of 'Takeable'.
--   Take elements form the tree using in-order traversal.
instance Takeable [] where
  takeSome = take

instance Takeable Tree where
  takeSome n = take n . elems

-- | 2.2 a
--   Define a 'Headed' class with a function which takes the
--   head of the structure, and a function which removes the
--   head and returns the rest.
class Headed t where
  headOf :: t a -> a
  headOff :: t a -> t a

-- | 2.2 b
--   Define '[]', 'Tree', and 'Maybe' as instances of this type class.
instance Headed [] where
  headOf = head
  headOff = tail

instance Headed Maybe where
  headOf Nothing  = error "Nothing"
  headOf (Just x) = x

  headOff Nothing = error "Nothing"
  headOff _       = Nothing

-- | Head of the tree in the sense of in-order traversal.
instance Headed Tree where
  headOf Null            = error "empty tree"
  headOf (Node x Null _) = x
  headOf (Node _ l _)    = headOf l

  headOff Null               = error "empty tree"
  headOff (Node _ Null Null) = Null
  headOff (Node _ Null r)    = r
  headOff (Node x l r)       = Node x (headOff l) r

-- | 3
instance Functor Tree where
  fmap _ Null         = Null
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- | 3.1
--   Using 'fmap' define a function which will apply 'f' to each
--   element of a tree of the 'Tree (Maybe a)' type.
mapOnTreeMaybe :: (a -> b) -> Tree (Maybe a) -> Tree (Maybe b)
mapOnTreeMaybe f = fmap (fmap f)

-- | 3.2 a
--   Define a 'RoseTree' type for trees in which each node can have
--   a number of subtrees (a forest). As type constructors, use
--   'RoseTree' and 'RoseEmpty'.
type RoseForest a = [RoseTree a]
data RoseTree a = RoseEmpty | RoseTree { val :: a, forest :: RoseForest a }

-- | 3.2 b
--   Make 'RoseTree' an instance of the 'Functor' class.
instance Functor RoseTree where
  fmap _ RoseEmpty = RoseEmpty
  fmap f (RoseTree x ts) = RoseTree (f x) ts'
    where
      ts' = map (fmap f) ts

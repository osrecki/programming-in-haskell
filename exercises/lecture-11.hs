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

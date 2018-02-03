{-|
Module      : Lecture9Exercises
Description : Solutions to in-class exercises for Lecture 9
Maintainer  : Dinko Osrecki
-}
module Lecture9Exercises where

-- | 1.1
--   Define a 'Date' structure with the appropriate fields.
--   Define a function that shows a date in the DD.MM.YYYY
--   format (without leading zeroes).
data Date = Date { day   :: Int
                 , month :: Int
                 , year  :: Int
                 } deriving (Show)

showDate :: Date -> String
showDate Date {day = d, month = m, year = y} =
  show d ++ "." ++ show m ++ "." ++ show y

-- | 1.2
--   Define a function 'translate :: Point -> Shape -> Shape'
--   which translates a shape in the direction of vector (a,b).
--
--   Rectangle is defined with 2 points where the first one
--   represents the bottom left corner, and the second one
--   the top right corner.
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

translate :: Point -> Shape -> Shape
translate (Point a b) (Circle (Point x y) r) =
  Circle (Point (x+a) (y+b)) r

translate (Point a b) (Rectangle (Point x1 y1) (Point x2 y2)) =
  Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- | 1.3 a
--   Write a function which tests whether a point is contained
--   within a given shape (or is on its border).
inShape :: Shape -> Point -> Bool
inShape (Circle (Point x y) r) (Point a b) = d2 <= r2
  where
    d2 = (a-x)**2 + (b-y)**2
    r2 = r**2

inShape (Rectangle (Point x1 y1) (Point x2 y2)) (Point a b) =
  x1 <= a && a <= x2 && y1 <= b && b <= y2

-- | 1.3 b
--   Write a function which tests if the point is within any shape
--   from the list of shapes.
inShapes :: [Shape] -> Point -> Bool
inShapes xs p = any (`inShape` p) xs

-- | 1.4 a
--   Define your type 'Vehicle' that can be a 'Car', 'Truck',
--  'Motorcycle', or 'Bicycle'. The first three store a name
--   of the manufacturer (String) and horsepower (Double).
data Vehicle = Car String Double
             | Truck String Double
             | Motorcycle String Double
             | Bicycle
             deriving (Show)

-- | 1.4 b
--   Write a function 'totalHorsepower' that adds up the horsepower
--   of the vehicles, assuming that bicycle's horsepower is 0.2.
totalHorsepower :: [Vehicle] -> Double
totalHorsepower = sum . map horsepower

horsepower :: Vehicle -> Double
horsepower (Car _ h)        = h
horsepower (Truck _ h)      = h
horsepower (Motorcycle _ h) = h
horsepower Bicycle          = 0.2

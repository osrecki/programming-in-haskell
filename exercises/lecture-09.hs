{-|
Module      : Lecture9Exercises
Description : Solutions to Lecture 9 exercises
Maintainer  : Dinko Osrecki
-}
module Lecture9Exercises where

import           Data.List
import           Data.Maybe
import           Data.Ord

-- EXERCISE 01 ----------------------------------------------------------------

{-
  1.1
  - Define a 'Date' structure with the appropriate fields.
    Define a function which shows a date in the DD.MM.YYYY format (without
    leading zeroes).
-}
data Date = Date { day   :: Int
                 , month :: Int
                 , year  :: Int
                 } deriving (Show)

showDate :: Date -> String
showDate Date {day = d, month = m, year = y} =
  show d ++ "." ++ show m ++ "." ++ show y

{-
  1.2
  - Define a function 'translate :: Point -> Shape -> Shape' which translates
    a shape in the direction of vector (a,b).

    Rectangle is defined with 2 points where the first one represents the
    bottom left corner, and the second one the top right corner.
-}
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

translate :: Point -> Shape -> Shape
translate (Point a b) (Circle (Point x y) r) =
  Circle (Point (x+a) (y+b)) r

translate (Point a b) (Rectangle (Point x1 y1) (Point x2 y2)) =
  Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

{-
  1.3 a
  - Write a function which tests whether a point is contained within a given
    shape (or is on its border).
-}
inShape :: Shape -> Point -> Bool
inShape (Circle (Point x y) r) (Point a b) = d2 <= r2
  where
    d2 = (a-x)**2 + (b-y)**2
    r2 = r**2

inShape (Rectangle (Point x1 y1) (Point x2 y2)) (Point a b) =
  x1 <= a && a <= x2 && y1 <= b && b <= y2

{-
  1.3 b
  - Write a function which tests if the point is within any shape from the
    list of shapes.
-}
inShapes :: [Shape] -> Point -> Bool
inShapes xs p = any (`inShape` p) xs

{-
  1.4 a
  - Define your type 'Vehicle' that can be a 'Car', 'Truck', 'Motorcycle', or
    'Bicycle'. The first three store a name of the manufacturer (String) and
    horsepower (Double).
-}
type Manufacturer = String
type Horsepower = Double
data Vehicle = Car Manufacturer Horsepower
             | Truck Manufacturer Horsepower
             | Motorcycle Manufacturer Horsepower
             | Bicycle
             deriving (Show)

{-
  1.4 b
  - Write a function 'totalHorsepower' which adds up the horsepower of the
    vehicles, assuming that bicycle's horsepower is 0.2.
-}
totalHorsepower :: [Vehicle] -> Horsepower
totalHorsepower = sum . map horsepower

horsepower :: Vehicle -> Horsepower
horsepower (Car _ h)        = h
horsepower (Truck _ h)      = h
horsepower (Motorcycle _ h) = h
horsepower Bicycle          = 0.2

-- EXERCISE 02 ----------------------------------------------------------------

data Level = Bachelor | Master | PhD deriving (Show, Eq, Enum, Ord)
data Student = Student { studentId :: String
                       , firstName :: String
                       , lastName  :: String
                       , level     :: Level
                       , avgGrade  :: Double
                       } deriving (Show)

{-
  2.1
  - Define a function which increases the average grade of the student by 1.0,
    but not above 5.0.
-}
improveStudent :: Student -> Student
improveStudent s = s { avgGrade = g }
  where
    g = min 5 (avgGrade s + 1)

{-
  2.2
  - Write a function to compute the average grade of students for the different
    study levels.
-}
avgGradePerLevels :: [Student] -> (Double, Double, Double)
avgGradePerLevels xs = (avgGradePerLevel Bachelor xs,
                        avgGradePerLevel Master xs,
                        avgGradePerLevel PhD xs)

avgGradePerLevel :: Level -> [Student] -> Double
avgGradePerLevel l = average . map avgGrade . filter ((==) l . level)

average :: (Fractional a) => [a] -> a
average xs = sum xs / genericLength xs

{-
  2.3
  - Write a function which returns a list of matriculation numbers for a given
    study level, sorted by average grade in descending order.
-}
rankedStudents :: Level -> [Student] -> [String]
rankedStudents l =
  map studentId . sortOn (Down . avgGrade) . filter ((==) l . level)

{-
  2.4
  - Write a function which adds a student to a list of students. If a student
    with an identical matriculation number already exists in the list, the
    function should return an error.
-}
addStudent :: Student -> [Student] -> [Student]
addStudent s xs
  | existsStudent s xs = error "duplicate matriculation number"
  | otherwise          = s:xs

existsStudent :: Student -> [Student] -> Bool
existsStudent s = elem (studentId s) . map studentId

-- EXERCISE 03 ----------------------------------------------------------------

{-
  3.1 a
  - Define your own parametrized type 'MyTriplet' which contains the values of
    three different types. Do this using a record.
-}
data MyTriplet a b c = MyTriplet { _1 :: a
                                 , _2 :: b
                                 , _3 :: c
                                 } deriving (Show)

{-
  3.1 b
  - Define a function which converts a 'MyTriplet' value into an ordinary
    triplet.
-}
toTriplet :: MyTriplet a b c -> (a, b, c)
toTriplet t = (_1 t, _2 t, _3 t)

{-
  3.2
  - Define a function which sums the known salaries of employees (salaries
    that are not 'Nothing').
-}
data Employee = Employee { name   :: String
                         , salary :: Maybe Double
                         } deriving (Show)

totalSalaries :: [Employee] -> Double
totalSalaries = sum . map (fromMaybe 0 . salary)

{-
  3.3 a
  - Write a function 'addStudent2' that works like 'addStudent' from task 2.4,
    but returns a 'Maybe' instead of an error.
-}
addStudent2 :: Student -> [Student] -> Maybe [Student]
addStudent2 s xs
  | existsStudent s xs = Nothing
  | otherwise          = Just $ s:xs

{-
  3.3 b
  - Write 'addStudent3' that returns an 'Either'.
-}
addStudent3 :: Student -> [Student] -> Either String [Student]
addStudent3 s xs
  | existsStudent s xs = Left "duplicate matriculation number"
  | otherwise          = Right $ s:xs

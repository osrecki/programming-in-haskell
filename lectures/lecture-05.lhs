University of Zagreb
Faculty of Electrical Engineering and Computing

PROGRAMMING IN HASKELL

Academic Year 2017/2018

LECTURE 5: Recursive functions 1

v1.2

(c) 2017 Jan Snajder

==============================================================================

> import Data.Char
> import Data.List

== RECAP =====================================================================

Last week we've covered the full syntax of Haskell functions. You now know how
to write arbitrary complex functions, at least in terms of syntax. However, to
really unleash the full computational power of Haskell, you need to know how
to write recursive functions.

== INTRO =====================================================================

Recursion: function calls itself.

In FP, many problems are solved using recursion. The main idea: divide the
problem into smaller subproblems and try to solve these subproblems as simplest
cases first.

The simplest case is called "the base case".

A common example is the factorial function:

> fact :: (Eq a, Num a) => a -> a
> fact x = if x==0 then 1 else x * fact (x-1)

Or, better:

> fact' :: (Eq a, Num a) => a -> a
> fact' 0 = 1
> fact' x = x * fact' (x-1)

Another typical example is the Fibonacci Number:

> fib 0 = 0
> fib 1 = 1
> fib n = fib (n-1) + fib (n-2)

(This definition is not the best one. Do you know why?)

Haskellers are quite proud of the quicksort definition:

> quicksort :: Ord a => [a] -> [a]
> quicksort [] = []
> quicksort (x:xs) = quicksort ys ++ [x] ++ quicksort zs
>   where ys = [y | y <- xs, y <= x]
>         zs = [z | z <- xs, z  > x]

In what follows, we focus on recursion over a data structure. This is typically
a LIST or a TREE, but more generally it can be any recursive structure. We call
such a recursion STRUCTURAL RECURSION. We use structural recursion to process a
data structure (something we would do using a loop in an imperative language).

The main idea: recurse down the structure by gradually decomposing it using
pattern matching, and combining the results:

> sum' :: Num a => [a] -> a
> sum' []     = 0
> sum' (x:xs) = x + sum' xs

> length' :: [a] -> Int
> length' []     = 0
> length' (_:xs) = 1 + length' xs

> incList :: Num a => [a] -> [a]
> incList []     = []
> incList (x:xs) = (x + 1) : incList xs

(The last function can be defined via list comprehension. How?)

> concat' :: [[a]] -> [a]
> concat' []       = []
> concat' (xs:xss) = xs ++ concat' xss

> maximum' :: Ord a => [a] -> a
> maximum' [x]    = x
> maximum' (x:xs) = x `max` maximum' xs

(What would happen if we were to apply this function to an empty list?)

What is the time complexity of the above functions? (Except for
the 'fib' and 'quicksort' functions.)

For lists of length 'n', the time complexity is O(n).

Notice that there is a recurring pattern in the above functions:

foo ...                             <-- base case
foo (x:xs) = f x `operator` foo xs  <-- general case

== EXERCISE 1 ================================================================

1.1.
- Define a recursive function to compute the product of a list of elements.

1.2.
- Define a recursive function 'headsOf' that takes a list of lists and
  returns a list of their heads.
  headsOf :: [[a]] -> [a]
  headsOf [[1,2,3],[4,5],[6]] => [1,4,6]

==============================================================================

A recursive function can of course have many arguments. Arguments that remain
unchanged throughout the recursive calls serve merely to store the state. We
call such arguments CONTEXT VARIABLES. For example:

> addToList :: Num a => a -> [a] -> [a]
> addToList _ []     = []
> addToList n (x:xs) = x + n : addToList n xs

Of course, if required, we can change the variables in each recursive call:

> incIncList :: Num a => a -> [a] -> [a]
> incIncList _ []     = []
> incIncList n (x:xs) = x + n : incIncList (n + 1) xs

What if we wanted to define a function that increments the first element by 0,
the second by 1, etc.?

incIncList' [3,2,1] => [3,3,3]

We don't want the user to always provide 0 as the argument. Instead, we define
a WRAPPER FUNCTION:

> incIncList' :: Num a => [a] -> [a]
> incIncList' xs = inc 0 xs
>   where inc _ []     = []
>         inc n (x:xs) = x + n : inc (n+1) xs

== EXERCISE 2 ================================================================

2.1.
- Define a recursive function 'modMult n m xs' that multiplies each element of
  a list 'xs' with 'n' modulo 'm'.

2.2.
- Define a function 'addPredecessor' that adds to each element of a list the
  value of the preceding element. The first element gets no value added.
  addPredecessor :: Num a => [a] -> [a]
  addPredecessor [3,2,1] => [3,5,3]

> addPredecessor :: Num a => [a] -> [a]
> addPredecessor [] = []
> addPredecessor [x] = [x]
> addPredecessor (x:y:xs) = x : addPredecessor (y+x:xs)

==============================================================================

In the recursive case we can test additional conditions and act based on these.
This is how we can implement the filtering of a list:

> numPositives :: (Num a, Ord a) => [a] -> Int
> numPositives []     = 0
> numPositives (x:xs) | x > 0     = 1 + numPositives xs
>                     | otherwise = numPositives xs

== EXERCISE 3 ================================================================

3.1.
- Define 'equalTriplets' that filters from a list of triplets (x,y,z) all
  triplets for which x==y==z.
  equalTriplets [(1,2,3),(2,2,2),(4,5,6)] => [(2,2,2)]

3.2.
- Define your own version of the replicate function:
  replicate' :: Int -> a -> [a]

==============================================================================

Let's define 'take':

> take' :: Int -> [a] -> [a]
> take' 0 _      = []
> take' _ []     = []
> take' n (x:xs) = x : take' (n-1) xs

Does this work as expected if n<0 (does it return an unaltered list)?

How can we extend the above definition so that, if n > length xs, the last
element of the list gets repeated?
take'' 5 [1,2,3] => [1,2,3,3,3]

How would you define the same function using standard functions from Prelude?

== EXERCISE 4 ================================================================

4.1.
- Define your own recursive version of the drop function:
  drop' :: Int -> [a] -> [a].
- Define drop'' (a wrapper function) so that for n < 0 the function drops
  the elements from the end of the list. You can use 'reverse'.

4.2.
- Define a recursive function 'takeFromTo n1 n2 xs'.
  takeFromTo :: Int -> Int -> [a] -> [a]

==============================================================================

Here's how 'zip' function is defined:

> zip' :: [a] -> [b] -> [(a, b)]
> zip' []     _      = []
> zip' _      []     = []
> zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

How can we extend this so that it only pairs up (x,y) for which x==y?

We don't always need to process the elements one by one. For example, a
function that takes a list and pairs up the consecutive elements would be
defined like this:

> pairUp :: [a] -> [(a,a)]
> pairUp (x:y:xs) = (x, y) : pairUp xs
> pairUp _        = []

== EXERCISE 5 ================================================================

5.1.
- Define a recursive function 'eachThird' that retains every third element
  in a list.
  eachThird :: [a] -> [a]
  eachThird "zagreb" => "gb"

5.2.
- Define a recursive function 'crossZip' that zips two lists in a "crossing"
  manner:
  crossZip [1,2,3,4,5] [4,5,6,7,8] => [(1,5),(2,4),(3,7),(4,6)]

==============================================================================

Let's have a look at two more exotic function.

First is 'reverse'. How would you go about defining the reverse function
recursively?

Here the solution:

> reverse1 :: [a] -> [a]
> reverse1 []     = []
> reverse1 (x:xs) = reverse1 xs ++ [x]

Space complexity is O(n) but time complexity is as much as O(n^2).
Can you say why?

This is bad. We'll see next week how to improve on this and write an
alternative definition to reduce both the space and time complexity.

Another function is 'unzip':

> unzip'' :: [(a,b)] -> ([a],[b])
> unzip'' []         = ([], [])
> unzip'' ((x, y):zs) = (x:xs, y:ys)
>   where (xs, ys) = unzip'' zs

== CORECURSION ===============================================================

Corecursion is "dual" to recursion: instead of decomposing a structure, we
build it up. In RECURSION, each recursive call is applied to a structure that
is smaller than the input structure. Conversely, in CORECURSION, the recursive
call is applied to a larger structure than the input structure and there is no
base case. The structure that we build up can be finite or infinite. Of course,
because of laziness, we will build only as much as needed.

> ones :: [Integer]
> ones = 1 : ones

> cycle' :: a -> [a]
> cycle' x = x : cycle' x

In each step we can use a part of the already constructed structure.

List of natural numbers:

> nats :: [Integer]
> nats = 0 : next nats
>   where next (x:xs) = x + 1 : next xs

A bit more complex: a list of Fibonacci Numbers:

> fibs :: [Integer]
> fibs = 0 : 1 : next fibs
>   where next (x:ys@(y:_)) = (x+y) : next ys

More details here:
http://programmers.stackexchange.com/questions/144274/whats-the-difference-between-recursion-and-corecursion

== NEXT ======================================================================

Next week, we'll continue talking about recursion, and also look into how to
make Haskell less lazy and more strict.

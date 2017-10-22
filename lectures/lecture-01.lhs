University of Zagreb
Faculty of Electrical Engineering and Computing

PROGRAMMING IN HASKELL

Academic Year 2017/2018

LECTURE 1: Getting started

v1.1

(c) 2017 Jan Šnajder

==============================================================================

> import Data.Char
> import Data.List

=== THE BASICS ===============================================================

* Your favorite editor
* ghci
* program as a sequence of value/function definitions
* literate programming
* ghci commands

=== DEFINING VALUES AND FUNCTIONS ============================================

> x = 2

Functions are also values, so we can define them similarly. There are no
parentheses surrounding the variable:

> inc x = x + 1

Functions of many variables:

> digits2Number x y = x * 10 + y

So, don't write 'digits2Number(x,y)'. That is soooo non-Haskell!

We can now apply these functions. Again, the parentheses are dropped:

> y = inc 2
> z = digits2Number 4 2

Function names should be written with the initial letter in lowercase. Other
than that, the usual rules for identifiers apply.

Some built in functions: 'max', 'min', 'succ', 'div', 'mod'.

Infix format:

> w = 25 `div` 2

Note that, when you define values/functions in the interactive interpreter, you
have to put 'let' in front:

--> let x = 2
--> let inc x = x + 1

Why this is so will be clear by the end of the course.

=== STRINGS AND CHARACTERS ===================================================

> name = "Humpty Dumpty"

> letter = 'H'

Concatenating strings:

> s = "One " ++ "two " ++ "three"

You cannot concatenate letters! This won't work:

'a' ++ 'b'

Length will give you the length of a string:

> n1 = length "The quick brown fox jumps over the lazy dog"
> n2 = length s

=== IF-THEN-ELSE =============================================================

> condDec x = if x > 0 then x - 1 else x

> foo x = (if even x then x*2 else 2) + 1

Not the same as:

> foo' x = if even x then x*2 else 2 + 1

> bigNumber x = if x >= 1000 then True else False

Avoid explicitly returning True/False; instead, simply return the whole Boolean
expression.

> bigNumber' x = x >= 1000

Playing with strings a bit:

> merge s1 s2 =
>   s1 ++ (if s1 < s2 then " is not " else " is ") ++ s2

> merge2 s1 s2 =
>   s1 ++ " is " ++ (if s1 < s2 then "not " else "") ++ s2

=== GUARDS ===================================================================

> merge3 s1 s2
>   | s1 < s2   = s1 ++ " is not " ++ s2
>   | otherwise = s1 ++ " is " ++ s2

> grade score
>   | score < 50 = 1
>   | score < 63 = 2
>   | score < 76 = 3
>   | score < 89 = 4
>   | otherwise  = 5

> showSalary amount bonus
>   | bonus /= 0 = "Salary is " ++ show amount ++ ", and a bonus " ++
>                  show bonus
>   | otherwise  = "Salary is " ++ show amount

=== EXERCISE 1 ===============================================================

1.1.

- Define 'concat3' that concatenates three strings, but drops the middle one
  if it's shorter than 2 characters (use 'length' function).

1.2.
- Give a simpler definition of 'showSalary', using only one if-then-else
  construct.
- Additionally check that salary is non-negative. If it's negative, return an
  adequate message.

=== LISTS ====================================================================

> l1 = [1, 2, 3]

Operator ':' (so-called "cons"):

> l1' = (1:(2:(3:[])))
> l1'' = 1:2:3:[]

List concatenation:

> l2 = [1,2,3] ++ [4,5,6]

> myConcat l1 l2 = l1 ++ l2

Turning elements into singleton lists:

> listify x = [x]

> listify' x = x:[]

Extracting parts of a list: head, tail, init, last.

Taking or dropping the initial part of a list:

> l3 = take 3 [9,2,10,3,4]
> l4 = drop 3 [9,2,10,3,4]

Reversing a list:

> l5 = reverse [1,2,3]

Strings are lists of characters:

> l6 = "this is a list"

> l7 = head l6

> l8 = 'H' : "askell"

Is a string a palindrome?

> isPalindrome s = s == reverse s

=== NEXT =====================================================================

Next week we'll have two classes, to bring you up to speed as soon as possible.
In the first class, we'll continue talking about lists an tuples.

To prepare, you may (but need not to) read Chapter 2 of LYH:
http://learnyouahaskell.com/starting-out

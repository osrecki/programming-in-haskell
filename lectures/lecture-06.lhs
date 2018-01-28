University of Zagreb
Faculty of Electrical Engineering and Computing

PROGRAMMING IN HASKELL

Academic Year 2017/2018

LECTURE 6: Recursive functions 2

v1.1

(c) 2017 Jan Snajder

==============================================================================

> import Data.Char
> import Data.List

== ACCUMULATORS ==============================================================

Let's look at the definition of the factorial function again:

> fact1 :: (Eq a, Num a) => a -> a
> fact1 0 = 1
> fact1 x = x * fact1 (x-1)

This function is executed as follows: we go down until we "hit" the base case,
and then build up the result incrementally as we return from the recursive
calls. Actually, the result is built up while on our way back.

But another solution is possible, one in which we recurse down and "accumulate"
the solution incrementally as we descend. When we "hit" the base case, we
simply return the solution accumulated thus far. There is no need to go back,
because there is nothing left to be done on the way back. We can simply "jump
out" of the recursion.

> fact2 :: (Eq a, Num a) => a -> a -> a   -- the second arg. is the accumulator
> fact2 0 n = n
> fact2 x n = fact2 (x-1) (x*n)

We also need a wrapper function that will set the initial value (which equals
one):

> fact3 :: (Eq a, Num a) => a -> a
> fact3 x = fact2 x 1

All recursive function defined above were defined using "standard" recursion
(without the accumulator). Many of those can be defined using the accumulator.
For instance, instead of:

> sum1 :: Num a => [a] -> a
> sum1 []     = 0
> sum1 (x:xs) = x + sum1 xs

> sum2 :: Num a => [a] -> a
> sum2 xs = sum xs 0
>   where sum []     s = s --s is the accumulator
>         sum (x:xs) s = sum xs (x + s)

But why? Isn't it all the same?

It's not. In principle, the accumulator-style definitions are of less
complexity (space complexity, but sometimes also time complexity).

A somewhat lengthy explanation:

When a recursive call is a part of a larger expression, to be able to compute
the value of the expression we first have invoke the recursive function. It is
only after the recursive call returns that we can compute the value of the whole
expression. Conceptually, this means that we gradually build up a larger and
larger expression, and we can start reducing this expression only after we've
reached the base case. (Technically, this is accomplished by storing the
context and the return address on stack for each recursive call.) The
intermediate structure grows with each recursive call. If there is only one
recursive call, and it's invoked 'n' times, then the space complexity will be
O(n).

On the other hand, if the recursive call is NOT a part of a larger expression,
then no structure is built as we recurse down. In this case, we don't even need
to store the return address on stack. Instead, we can simply make the recursive
call with new parameters (as if were using a GOTO with parameters), because
there is nothing that is waiting to be computed AFTER the recursive call is
completed. We call such recursive functions TAIL RECURSIVE. The compiler will
detect that a function is tail recursive and optimize the code (TAIL CALL
OPTIMIZATION). Since we don't have to store anything on stack, the space
complexity is constant, O(1).

Brent Yorgey:
"A recursive function is tail recursive if the final result of the recursive
call is the final result of the function itself.  If the result of the
recursive call must be further processed (say, by adding 1 to it, or consing
another element onto the beginning of it), it is not tail recursive."
http://www.haskell.org/pipermail/haskell-cafe/2009-March/058607.html

Thus: 'sum1' has a space complexity of O(n), while 'sum2' has a space
complexity of O(1).

Let's define an accumulator-style version of 'maximum'. The standard version
is:

> maximum1 :: Ord a => [a] -> a
> maximum1 []  = error "empty list"
> maximum1 [x] = x
> maximum1 (x:xs) = x `max` maximum1 xs

Accumulator-style version:

> maximum2 :: (Ord a, Num a) => [a] -> a
> maximum2 [] = error "empty list"
> maximum2 xs = maximum xs 0
>   where maximum []     m = m
>         maximum (x:xs) m = maximum xs (max x m)

There's actually no need to limit ourselves to the 'Num' typeclass here, so
let's give a slightly more generic definition:

> maximum3 :: (Ord a, Bounded a) => [a] -> a
> maximum3 [] = error "empty list"
> maximum3 xs = maximum xs minBound
>   where maximum []     m = m
>         maximum (x:xs) m = maximum xs (max x m)

Actually, a more clever definition avoids using a bound altogether:

> maximum4 :: (Ord a) => [a] -> a
> maximum4 []     = error "empty list"
> maximum4 (x:xs) = maximum xs x
>   where maximum []     m = m
>         maximum (x:xs) m = maximum xs (max x m)

The difference between standard and accumulator-style recursion is quite
drastic in the case of the 'reverse' function. The standard definition is as
follows:

> reverse1 :: [a] -> [a]
> reverse1 []     = []
> reverse1 (x:xs) = reverse1 xs ++ [x]

Space complexity is O(n) but time complexity is as much as O(n^2).
Can you say why?

Accumulator-style version:

> reverse2 :: [a] -> [a]
> reverse2 xs = rev xs []
>   where rev []     ys = ys
>         rev (x:xs) ys = rev xs (x:ys)

What is the complexity of this function?

Another advantage of accumulator-style definitions is that in some cases it is
perhaps easier to comprehend the behavior of such functions (accumulator-style
definitions are closer to imperative-style programming).

So it seems that using accumulator-style recursion is a good idea. But there is
a caveat to it. Because Haskell is lazy, the accumulator will not be fully
evaluated and might grow gradually in memory, causing a MEMORY LEAKAGE. Thus,
using accumulators only makes sense if they are STRICT. More on this in later
lectures.

== EXERCISE 1 ================================================================

1.1.
- Write an accumulator-style recursive definition of
  length' :: [a] -> Int

1.2
- Write an accumulator-style recursive definition of
    maxUnzip :: [(Int, Int)] -> (Int, Int)
  that returns the maximum element at the first position and the maximum
  element at the second position in a pair, i.e., it's equivalent to:
    maxUnzip zs = (maximum xs, maximum ys)
      where (xs,ys) = unzip zs
  If the list is empty, return an "empty list" error.
- Now write a standard recursive definition (without an accumulator).

== GUARDED RECURSION ==========================================================

Sometimes, using an accumulator doesn't even make sense to begin with. For
example, if we do structural recursion on a list and modify each element in
turn. Look at the 'incList' function:

> incList1 :: Num a => [a] -> [a]
> incList1 []     = []
> incList1 (x:xs) = x + 1 : incList1 xs

The space complexity of this function is O(1). (The list that is being
constructed is not counted in the space complexity. Only additional memory
allocated during the computation is counted, but here no extra memory is being
allocated.)

We might give it a shot with an accumulator-style version:

> incList2 :: Num a => [a] -> [a]
> incList2 xs = inc xs []
>   where inc []     ys = ys
>         inc (x:xs) ys = inc xs ((x+1):ys)

The problem here is that we can only prepend to the list (to get an O(1)
operation) and thus the accumulated list will be in reverse order. This would
have been OK for 'reverse', but here it's no good. Moreover, accumulator-style
doesn't buy us anything, as the space complexity of the function already was
O(1).

Another example is 'unzip'. We may give it a try with accumulators (two in this
case):

> unzip' :: [(a,b)] -> ([a],[b])
> unzip' zs = unz zs [] []
>   where unz []          xs ys = (xs, ys)
>         unz ((x, y):zs) xs ys = unz zs (x:xs) (y:ys)

But this is again not good for the same reason as above: we end up with lists
in reverse order. We might first reverse the input list, but that would require
two list traversals (one for the reversal and one for unzipping).

Hence in this case too we should resort to "standard" recursion:

> unzip'' :: [(a,b)] -> ([a],[b])
> unzip'' []         = ([], [])
> unzip'' ((x, y):zs) = (x:xs, y:ys)
>   where (xs, ys) = unzip'' zs

The two functions above ('incList' and 'unzip') have one thing in common: we
use recursion to create the output list(s) incrementally. The result is
immediately becoming available and continues to grow as the recursion
progresses. Because Haskell is LAZY, if we choose to consume just the first
part of the result, the recursion will never generate results beyond that point
since these results are not really needed. In other words, the result of the
function can be CONSUMED LAZILY. This is called GUARDED RECURSION. In guarded
recursion, the recursive call occurs within a "data constructor" (cons operator
':' in this case). Because of laziness, the expression will be evaluated up to
the data constructor and the recursive call delayed until needed.

Notice that guarded recursion is not tail recursive. However, there is nothing
left to be done after exiting the recursive call, so space complexity is O(1).
Hence we call such recursion TAIL RECURSION MODULO CONS.

SUMMARY:

Tail recursion reduces space complexity. To achieve it, use:

* Accumulator-style, but only if you need the whole result (e.g., sum, max,
  length, etc.).

* If you don't need the whole result at once but wish to consume it lazily,
  use guarded recursion, which will give you tail recursion modulo cons.

== STRICTNESS ================================================================

Haskell is a lazy language. It won't evaluate an expression until this is
required.

> e1 = head [1, 2..]

How does that work? Instead of evaluating the complete list, Haskell generates
a so-called THUNK or SUSPENSION -- a pointer to the expression and a data
structure containing all required to evaluate the expression. A thunk will only
be evaluated when required.

Another example of laziness:

  (&&) :: Bool -> Bool -> Bool
  False && _ = False
  True  && x = x

This function does not evaluate its second argument unless the first argument
is 'True'. We say that the function is NON-STRICT in its second argument. This
means that 'False && undefined' will work, but 'undefined && False' won't.
Similarly, function (||) is non-strict in its second argument, so 'True ||
undefined' will work.

A side note:
We've been a bit sloppy here because we equate NON-STRICTNESS with LAZINESS.
Strictness is a concept from lambda calculus. NON-STRICTNESS means an
expression can be reduced to a value, even if some of its parts are undefined.
LAZY EVALUATION is one of the possible ways to implement non-strictness. It
means that we only evaluate expressions when needed, and create thunks in the
meantime. Read more here:
http://www.haskell.org/haskellwiki/Lazy_vs._non-strict

While in most cases we want the evaluation to be lazy, occasionally lazy
evaluation becomes problematic. For example, we absolutely need lazy evaluation
for this to work:

> filterOdd :: [a] -> [a]
> filterOdd xs = [x | (i, x) <- zip [0..] xs, odd i]

Another, perhaps more insightful example. Recall the sum function:

> sumA :: Num a => [a] -> a
> sumA xs = sum xs 0
>   where sum []     s = s
>         sum (x:xs) s = sum xs (x + s)

> e2 = sumA [0..100]
> e3 = sumA [0..1000000000]

Note that we're using accumulator here, so this is supposed to be space
efficient because it's tail recursive. But despite this, we have a problem here
because '(x + s)' won't really be evaluated until it is needed.

Instead, it will build up thunks:

  sumA [0..10] =>
  => sum (0:[1..10]) 0
  => sum (1:[2..10]) (0+0)
  => sum (2:[3..10]) (1+(0+0))
  => sum (3:[4..10]) (2+(1+(0+0)))
  => sum (4:[5..10]) (3+(2+(1+(0+0))))
  => ...

This causes MEMORY LEAKAGE. The expression builds up in memory, consuming more
and more space, until it finally gets evaluated. The problem is that the thunks
consume more memory than the values they would evaluate to. To prevent this
from happening, we need a way to FORCE the evaluation of '(x + s)'.

There's a function that does that:

  seq :: a -> b -> b

Function 'seq' evaluates its first argument before it returns its second
argument.

For example:

> e4 = let x = undefined in 2
> e5 = let x = undefined in x `seq` 2
> e6 = let x = undefined in x `seq` snd (x, 5)

We can now define a strict version of sumA:

> sumA' :: Num a => [a] -> a
> sumA' xs = sum xs 0
>   where sum []     s = s
>         sum (x:xs) s = let a = x + s in a `seq` sum xs a

> e7 = sumA' [0..1000000000]

We can also define a strict version of the application operator ($):

  ($!) :: (a -> b) -> a -> b
  f $! x = x `seq` f x

'f $! x' will first evaluate the argument 'x', and then apply a function to it.

For example:

> e8 = let x = undefined in const 5 x
> e9 = let x = undefined in const 5 $! x

It's important to understand that 'seq' does not evaluate "too deep". If
expressions have structure, 'seq' will only "scratche the surface".

For example:

> e10 = let x = (undefined, 42) in x `seq` snd x

Here, 'seq' evaluated only the outermost structure (which is the pair
constructor), and did not proceed to evaluate the actual content of the pair.

When this is not enough, we need to make sure that 'seq' is applied recursively
to subexpressions. You can use the 'deepseq' package for this:
http://hackage.haskell.org/package/deepseq

== NEXT ======================================================================

In Haskell, everything's a function. Next, we'll look at HIGHER ORDER FUNCTIONS
(HOF), which are functions that take or return other functions. HOF allow for
functional design patterns, which make our code more structured, more modular,
and more comprehensible.

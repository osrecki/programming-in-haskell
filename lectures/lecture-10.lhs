University of Zagreb
Faculty of Electrical Engineering and Computing

PROGRAMMING IN HASKELL

Academic Year 2017/2018

LECTURE 10: Custom data types 2

v1.1

(c) 2017 Jan Šnajder

==============================================================================

> import Data.List
> import Control.Monad

=== INTRO ====================================================================

Last week we introduced the 'data' keyword for defining custom data types:
algebraic data types, records, and polymorphic data types. Today we extend on
this and look into recursive data types. In particular, we look into
polymorphic recursive types, such as lists and trees, which are important
because they serve as data containers.

=== RECURSIVE DATA STRUCTURES ================================================

Data structures can be recursive. In fact, the most useful data structures are
recursive.

> data Sex = Male | Female deriving (Show,Read,Ord,Eq)

> data Person = Person {
>   idNumber :: String,
>   forename :: String,
>   surname  :: String,
>   sex      :: Sex,
>   age      :: Int,
>   partner  :: Maybe Person,
>   children :: [Person] } deriving (Show,Read,Ord) -- Eq

Let's look at one family situation: Pero and Ana are Marko's parents, Marko and
Maja are dating...

> pero  = Person "2323" "Pero"  "Perić" Male   45 (Just ana)   [marko]
> ana   = Person "3244" "Ana"   "Anić"  Female 43 (Just pero)  [marko,iva]
> marko = Person "4341" "Marko" "Perić" Male   22 (Just maja)  []
> maja  = Person "7420" "Maja"  "Majić" Female 20 (Just marko) []
> iva   = Person "4642" "Iva"   "Ivić"  Female 16 Nothing      []

Now try to print out the value of 'pero'. What's happening?

'pero' (and other values we've defined) are infinite recursive structure. This
is because Pero is Ana's partner, while Ana's partner is Pero, who's partner is
Ana again, etc. If we were to represent these relationships with a graph, it
would have cycles. If a graph has cycles, the corresponding data structure is
infinite!

What's going on here?

  pero == ana
  pero == pero

The first comparison works just fine, while the second one never terminates
because 'pero' is an infinite structure. (Why does the first computation work
then?)

Let's write a function to return a name of one's partner, if such exists:

> partnersForename :: Person -> Maybe String
> partnersForename p = case partner p of
>   Just p  -> Just $ forename p
>   Nothing -> Nothing

or shorter:

> partnersForename2 :: Person -> Maybe String
> partnersForename2 = fmap forename . partner

Children of both partners:

> pairsChildren :: Person -> [Person]
> pairsChildren p = nub $ children p ++ case partner p of
>   Nothing -> []
>   Just r  -> children r

or shorter:

> pairsChildren2 :: Person -> [Person]
> pairsChildren2 p = nub $ children p ++ maybe [] children (partner p)

Will this work?

Nope. The problem is that 'nub' will compare the elements of the list. If there
are two equal elements, this computation will never terminate.

And now for something completely different. How would you go about defining the
following function:

  partnersMother :: Person -> Maybe Person

Unfortunately, there's no way to do this because we have no link back to one
person's parents. We need to either add this backlink, or put all persons in a
list and then search the list for parents of a given person.

Here's the first approach:

> data Person2 = Person2 {
>   personId2 :: String,
>   forename2 :: String,
>   surname2  :: String,
>   sex2      :: Sex,   --- data Sex = Male | Female deriving (Show,Read,Eq,Ord)
>   mother2   :: Maybe Person2,
>   father2   :: Maybe Person2,
>   partner2  :: Maybe Person2,
>   children2 :: [Person2] } deriving (Show,Read,Eq,Ord)

> john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) []
> jane = Person2 "623" "Jane" "Fox" Female (Just ann) Nothing (Just john) []
> ann  = Person2 "343" "Ann"  "Doe" Female Nothing Nothing Nothing [jane]

=== EXERCISE 1 ===============================================================

1.1.
- Define a function

1.2.
- Define a function
  parentCheck :: Person2 -> Bool
  that checks whether the given person is one of the children of its parents.

1.3.
- Define a function
  sister :: Person2 -> Maybe Person2
  that returns the sister of a person, if such exists.

1.4.
- Define a function that returns all descendants of a person.
  descendant :: Person2 -> [Person2]

==============================================================================

We already know that a list is also a recursive structure. Moreover, it is a
parametrized recursive structure. Let's define our own list data type:

> data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord,Eq)

Now we can define some lists:

> l1 = 1 `Cons` Empty
> l2 = 1 `Cons` (2 `Cons` (3 `Cons` Empty))

To improve readability, we can define our own infix operator:

> infixr 5 -+-
> (-+-) = Cons

Operator priority (set to 5 in the above example) range from 0 (lowest
priority) to 9 (highest priority). E.g., ($) has a priority 0, while (.) has
priority 9. You can find out more here:
http://www.haskell.org/onlinereport/decls.html#fixity

We can now write:

> l3 = 1 -+- 2 -+- 3 -+- Empty

=== EXERCISE 2 ===============================================================

2.1.
- Define
  listHead :: MyList a -> Maybe a

2.2.
- Define a function that works like 'map' but works on a 'MyList' type:
  listMap :: (a -> b) -> MyList a -> MyList b

==============================================================================

A prototypical example of a recursive data structure is a tree.

Here's a binary tree that stores the values in its inner nodes:

> data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Eq)

E.g., a binary tree of integers:

> intTree :: Tree Int
> intTree = Node 1 (Node 2 Null Null) (Node 3 Null Null)

A function that sums the elements in a binary tree of integers:

> sumTree :: Tree Int -> Int
> sumTree Null                = 0
> sumTree (Node x left right) = x + sumTree left + sumTree right

A function that tests whether an element is contained in a tree:

> treeElem :: Eq a => a -> Tree a -> Bool
> treeElem _ Null = False
> treeElem x (Node y left right)
>   | x == y    = True
>   | otherwise = treeElem x left || treeElem x right

=== EXERCISE 3 ===============================================================

3.1.
- Define a function
  treeMax :: Ord a => Tree a -> a
  that finds the maximum element in a tree. Return an error if the tree is
  empty.

3.2.
- Define a function
  treeToList :: Ord a => Tree a -> [a]
  that will collect in a list all elements from inner nodes of a tree by doing
  an in-order (left-root-right) traversal.

3.3.
- Define a function to prune the tree at a given level (root has level 0).
  levelCut :: Int -> Tree a -> Tree a

==============================================================================

A sorted tree (binary search tree): for each node containing value 'x', the
left subtree contains values that are less than 'x', while the right subtree
contains values that are greater than 'x'.

Insertion into a binary search tree:

> treeInsert :: Ord a => a -> Tree a -> Tree a
> treeInsert x Null = Node x Null Null
> treeInsert x t@(Node y l r)
>   | x < y     = Node y (treeInsert x l) r
>   | x > y     = Node y l (treeInsert x r)
>   | otherwise = t

=== EXERCISE 4 ===============================================================

4.1.
- Define a function that converts a list into a sorted tree.
  listToTree :: Ord a => [a] -> Tree a

4.2.
- Using 'listToTree' and 'treeToList' defined previously, define these two
  functions, define:
  sortAndNub :: Ord a => [a] -> [a]

=== DERIVING TYPE CLASS INSTANCES ============================================

We've already seen that Haskell can automatically derive instances for main
type classes: Eq, Ord, Show, Read, ...

Recall an earlier example:

  data Person = Person {
    idNumber :: String,
    forename :: String,
    surname  :: String,
    sex      :: Sex,
    age      :: Int,
    partner  :: Maybe Person,
    children :: [Person] } deriving (Show,Read,Eq,Ord)

We can do:

> t1 = marko == ana
> t2 = ana > marko
> t3 = compare ana marko
> ps = sort [marko,ana,pero]

If a type is an instance of the 'Read' type class, we can read in its values
from a string:

> s = read "Male" :: Sex

> p3 = read $ "Person {idNumber=\"111\",forename=\"Ivo\",surname=\"Ivic\"," ++
>             "sex=Male,age=11,partner=Nothing,children=[]}" :: Person

The built-in 'read' assumes that the string conforms to the Haskell syntax.
Similarly, build-in 'show' outputs the strings in Haskell syntax. A user can of
course redefine 'read' and 'show', but doing so is recommended only is a few
cases (as we'll see later). Note that 'read' and 'show' are very useful for
data serialization.

The 'Enum' type class allows for enumerating:

> data Weekday =
>   Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
>   deriving (Show,Enum)

> yesterday :: Weekday -> Weekday
> yesterday = pred

> dayAfterYesterday :: Weekday -> Weekday
> dayAfterYesterday = succ . pred

> workDays = [Monday .. Friday]

=== DEFINING TYPE CLASS INSTANCES ============================================

First and foremost, let's remind ourselves: type classes have nothing to do
with classes in OOP.

Now let's look at an example. We'd like to define a different kind of equality
test for our data type. E.g., we'd like to consider two persons to be identical
if they have the same national identification number. In this case we would not
need to check the other fields (which is good because the structure is
infinite). Similarly, we could define an ordering based on this number.

Let's have a look at how 'Eq' type class is defined:

  class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

    x == y = not (x /= y)
    x /= y = not (x == y)

There's the name of the type class, a type variable, and a list of functions
that must be defined for this type, in this case functions (==) and (/=). The
definitions themselves are not given here, only the type signatures.

We then can (but most not) have default definitions of functions, like it is
done in this case.

Having default definitions means that, when defining and instance, one will not
have to define all functions but can rely on default definitions. For example,
in this case it suffices to define (==), because there's a default definition
for (/=) that uses (==). So, for each type we only have to define those
functions that have no default definitions. This is called a MINIMAL COMPLETE
DEFINITION.

In case of 'Eq', the minimal complete definition is either the (==) function or
the (/=) function.

Let's look now at how we can define that a type is an instance of the 'Eq' type
class:

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday    == Friday    = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

Of course, this can become tedious, so that's why we can automatically derive
an instance.

We can now define our own 'Eq' instance for the 'Person' type:

> instance Eq Person where
>   p1 == p2 = idNumber p1 == idNumber p2

REMARK: We must now remove 'deriving Eq' from the definition of the 'Person'
type.

Now 'pero == pero' will work (check it out!).

Let's also define an instance 'Ord' type class. The minimal complete definition
is (<=). So it suffices to define:

 instance Ord Person where
   p1 <= p2 = idNumber p1 <= idNumber p2

REMARK: We can use the ":info" command in ghci to see a definition of a type
class and all its instances.

=== EXERCISE 5 ===============================================================

5.1.
- Define an 'Eq' instance for the 'Weekday' type that works like (==), except
  that two Fridays are never identical.

5.2.
- Define 'Person' as an instance of 'Show' type class so that instead of the
  values of partners and children only the respective person names are shown,
  which will enable the print out of an infinite structure of this type.

==============================================================================

What if we want to define an instance of a parametrized type?

That's not a problem. We simply have to provide a type variable together with
the type constructor. E.g.:

  instance Eq a => Eq (Maybe a)
    Just x  == Just y   = x == y
    Nothing == Nothing  = True
    _       == _        = False

A DIGRESSION: Why can't we simply write 'Maybe'? Because 'Eq' type class
expects a type, whereas 'Maybe' is not a type but a type constructor. In other
words, a type constructor 'Maybe' will produce a type only when given a type as
input.  E.g., 'Maybe Int' is a type. Similarly, 'Either' expects two types,
before it produces a type. So these type constructors themselves are of
different "types". We're talking about "types of types" here, which we call
KINDS in Haskell. 'Int', 'Maybe', and 'Either' are type constructors of
different kind. You can find out their kind using the ":kind" command in
ghci:

  Int :: *
  Maybe :: * -> *
  Either :: * -> * -> *

Kind '*' is just an ordinary type.
Kind '* -> *' is a unary type constructor, that takes in an ordinary type and
returns an ordinary type.

== EXERCISE 6 ================================================================

6.1.
- Define an instance of 'Eq' for 'MyList a' so that two lists are considered
  equal if they have the same first element.

6.2.
- Define an instance of 'Eq' for 'Tree a' so that two trees are considered
  equal if they store the same values, regardless of the position of these
  values in the trees, and regardless of duplicates.

=== NEXT =====================================================================

In the next lecture we'll look into custom types classes as well as standard
data types, such as sets, maps, trees, and graphs.

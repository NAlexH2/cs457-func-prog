> module Types where

This is a literate Haskell file (.lhs). Unlike the source files you usually see,
you write text as normal in literate Haskell files and you start code lines with
a ">" symbol. In this way, a line of code is more like a comment in an
"ordinary" file.

Outline for today's class.

Basic types
------------

- ()
- Bool
- Char
- String
- Int
- Integer
- Float
- Double

Lists
------------

- Lists
- Nested lists

Tuple and Either
------------

Function types
------------

Polymorphic types
------------

Overloaded types
------------

Basic classes
------------

- Eq
- Ord
- Show
- Read
- Num
- Integral
- Fractional

Intro to algebraic data types
------------

The purpose of this exercise is to provide some fun intuition about the
connection between Haskell and mathematics. You will learn more about algebraic
data types later in this course.

Let us start with the type that only has one inhabitant, we name it one:

> type One    = ()

Using One and Either, we can derive a type that has exactly two inhabitants:

> type Two    = Either One One

This type has two inhabitants: Left () and Right (), respectively. Note that
(One, One) only gives you another One.

> one_in_two = Left () :: Two
> two_in_two = Right () :: Two

Similarly, we derive Three:

> type Three  = Either Two One

Its inhabitants:

> one_in_three = Left (Left ()) :: Three
> two_in_three = Left (Right ()) :: Three
> three_in_three = Right () :: Three

We can also derive Three using Either One Two, of course.

The same for Four:

> type Four   = Either Two Two

But for Four, there is an alternative way:

> type Four'  = (Two, Two)

We have four inhabitants:

> one_in_four' = (Left (), Left ()) :: Four'
> two_in_four' = (Left (), Right ()) :: Four'
> three_in_four' = (Right (), Left ()) :: Four'
> four_in_four' = (Right (), Right ()) :: Four'

Similarly, we get Six as follow:

> type Six = (Three, Two)

Its inhabitants:

> one_in_six = (one_in_three, one_in_two) :: Six
> two_in_six = (one_in_three, two_in_two) :: Six
> three_in_six = (two_in_three, one_in_two) :: Six
> four_in_six = (two_in_three, two_in_two) :: Six
> five_in_six = (three_in_three, one_in_two) :: Six
> six_in_six = (three_in_three, two_in_two) :: Six

As we can see, Eithers "act like" sums and Tuples "act like" products. In
functional programming, An Either is usually referred to as a sum type; A
2-tuple is usually referred to as a product type.

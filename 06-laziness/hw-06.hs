{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-
Fibonacci numbers
The Fibonacci numbers Fn are defined as the sequence of integers,
beginning with 0 and 1, where every integer in the sequence is the
sum of the previous two. That is,
F0 = 0
F1 = 1
Fn = Fn−1 + Fn−2 (n ≥ 2)
For example, the first fifteen Fibonacci numbers are
0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, . . .
It’s quite likely that you’ve heard of the Fibonacci numbers before.
The reason they’re so famous probably has something to do with the
simplicity of their definition combined with the astounding variety of
ways that they show up in various areas of mathematics as well as art
and nature.

Exercise 1
Translate the above definition of Fibonacci numbers directly into a
recursive function definition of type
fib :: Integer -> Integer
so that fib n computes the nth Fibonacci number Fn.
Now use fib to define the infinite list of all Fibonacci numbers,
fibs1 :: [Integer]
(Hint: You can write the list of all positive integers as [0..].)
Try evaluating fibs1 at the ghci prompt. You will probably get
bored watching it after the first 30 or so Fibonacci numbers, because
fib is ridiculously slow. Although it is a good way to define the Fibonacci numbers, it is not a very good way to compute them—in order
to compute Fn it essentially ends up adding 1 to itself Fn times! For
example, shown at right is the tree of recursive calls made by evaluating fib 5.

As you can see, it does a lot of repeated work. In the end, fib
has running time O(Fn), which (it turns out) is equivalent to O(ϕn),
where ϕ = (1+√5)/2 is the “golden ratio”. That’s right, the running time
is exponential in n. What’s more, all this work is also repeated from
each element of the list fibs1 to the next. Surely we can do better.
-}
--Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 .. ]

{-
Exercise 2
When I said “we” in the previous sentence I actually meant “you”.
Your task for this exercise is to come up with more efficient implementation. Specifically, define the infinite list
fibs2 :: [Integer]
so that it has the same elements as fibs1, but computing the first n
elements of fibs2 requires only O(n) addition operations. Be sure to
use standard recursion pattern(s) from the Prelude as appropriate.
-}


--Exercise 2
fibs2 :: [Integer]
fibs2 = fibGen 0 1
  where
    fibGen :: Integer -> Integer -> [Integer]
    fibGen a b = a : fibGen b (a + b)


{-
Streams
We can be more explicit about infinite lists by defining a type Stream
representing lists that must be infinite. (The usual list type represents
lists that may be infinite but may also have some finite length.)
In particular, streams are like lists but with only a “cons” constructor—
whereas the list type has two constructors, [] (the empty list) and
(:) (cons), there is no such thing as an empty stream. So a stream is
simply defined as an element followed by a stream.

Exercise 3
• Define a data type of polymorphic streams, Stream.
• Write a function to convert a Stream to an infinite list,
streamToList :: Stream a -> [a]
• To test your Stream functions in the succeeding exercises, it will be
useful to have an instance of Show for Streams. However, if you put
deriving Show after your definition of Stream, as one usually does,
the resulting instance will try to print an entire Stream—which,
of course, will never finish. Instead, you should make your own
instance of Show for Stream, 

instance Show a => Show (Stream a) where
show ...
which works by showing only some prefix of a stream (say, the
first 20 elements).
-}
--Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons val tail) = val : streamToList tail

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList


{-
Exercise 4
Let’s create some simple tools for working with Streams.
• Write a function
streamRepeat :: a -> Stream a
which generates a stream containing infinitely many copies of the
given element.
• Write a function
streamMap :: (a -> b) -> Stream a -> Stream b
which applies a function to every element of a Stream.
• Write a function
streamFromSeed :: (a -> a) -> a -> Stream a
which generates a Stream from a “seed” of type a, which is the
first element of the stream, and an “unfolding rule” of type a -> a
which specifies how to transform the seed into a new seed, to be
used for generating the rest of the stream.
-}
--Exercise 4
streamRepeat :: a -> Stream a
streamRepeat val = Cons val (streamRepeat val)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap func (Cons val tail) = Cons (func val) (streamMap func tail)       

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed func val = Cons val (streamFromSeed func (func val))


{-
Exercise 5
Now that we have some tools for working with streams, let’s create a few:
• Define the stream
nats :: Stream Integer
which contains the infinite list of natural numbers 0, 1, 2, . . .
• Define the stream
ruler :: Stream Integer
which corresponds to the ruler function
0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
where the nth element in the stream (assuming the first element
corresponds to n = 1) is the largest power of 2 which evenly
divides n.
-}
--Exercise 5
nats :: Stream Integer
nats = streamFromSeed (\num -> num + 1) 0

ruler :: Stream Integer
ruler = initRulerFrom 0
  where 
    initRulerFrom :: Integer -> Stream Integer
    initRulerFrom num = interleaveStreams (streamRepeat num) (initRulerFrom $ num + 1)
      where
        interleaveStreams :: Stream a -> Stream a -> Stream a
        interleaveStreams (Cons aVal aTail) secStream 
            = Cons aVal (interleaveStreams secStream aTail)

{-
Fibonacci numbers via generating functions (extra credit)
This section is optional but very cool, so if you have time I hope you
will try it. We will use streams of Integers to compute the Fibonacci
numbers in an astounding way.
The essential idea is to work with generating functions of the form
a0 + a1x + a2x
2 + · · · + anx
n + . . .
where x is just a “formal parameter” (that is, we will never actually
substitute any values for x; we just use it as a placeholder) and all the
coefficients ai are integers. We will store the coefficients a0, a1, a2, . . .
in a Stream Integer.
Exercise 6 (Optional)
• First, define
x :: Stream Integer
by noting that x = 0 + 1x + 0x
2 + 0x
3 + . . . .
• Define an instance of the Num type class for Stream Integer.
Here’s what should go in your Num instance:
– You should implement the fromInteger function. Note that
n = n + 0x + 0x
2 + 0x
3 + . . . .
– You should implement negate: to negate a generating function,
negate all its coefficients.
– You should implement (+), which works like you would expect:
(a0 + a1x + a2x
2 + . . .) + (b0 + b1x + b2x
2 + . . .) = (a0 + b0) +
(a1 + b1)x + (a2 + b2)x
2 + . . .
– Multiplication is a bit trickier. Suppose A = a0 + xA0 and
B = b0 + xB0 are two generating functions we wish to multiply.
We reason as follows:
AB = (a0 + xA0
)B
= a0B + xA0B
= a0(b0 + xB0
) + xA0B
= a0b0 + x(a0B
0 + A
0B)
That is, the first element of the product AB is the product of
the first elements, a0b0; the remainder of the coefficient stream
(the part after the x) is formed by multiplying every element in
B
0
(that is, the tail of B) by a0, and to this adding the result of
multiplying A
0
(the tail of A) by B.

Note that there are a few methods of the Num class I have not
told you to implement, such as abs and signum. ghc will complain
that you haven’t defined them, but don’t worry about it. We won’t
need those methods. (To turn off these warnings you can add
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
to the top of your file.)
If you have implemented the above correctly, you should be able
to evaluate things at the ghci prompt such as
*Main> x^4
*Main> (1 + x)^5
*Main> (x^2 + x + 3) * (x - 5)
• The penultimate step is to implement an instance of the Fractional
class for Stream Integer. Here the important method to define is
division, (/). I won’t bother deriving it (though it isn’t hard), but
it turns out that if A = a0 + xA0 and B = b0 + xB0
, then A/B = Q,
where Q is defined as
Q = (a0/b0) + x((1/b0)(A
0 − QB0
)).
That is, the first element of the result is a0/b0; the remainder is
formed by computing A
0 − QB0 and dividing each of its elements
by b0.
Of course, in general, this operation might not result in a stream
of Integers. However, we will only be using this instance in cases
where it does, so just use the div operation where appropriate.
• Consider representing the Fibonacci numbers using a generating
function,
F(x) = F0 + F1x + F2x
2 + F3x
3 + . . .
Notice that x + xF(x) + x
2F(x) = F(x):
x
F0x + F1x
2 + F2x
3 + F3x
4 + . . .
F0x
2 + F1x
3 + F2x
4 + . . .
0 + x + F2x
2 + F3x
3 + F4x
4 + . . .
Thus x = F(x) − xF(x) − x
2F(x), and solving for F(x) we find
that
F(x) = x
1 − x − x
2
.
Translate this into an (amazing, totally sweet) definition
fibs3 :: Stream Integer

Fibonacci numbers via matrices (extra credit)
It turns out that it is possible to compute the nth Fibonacci number
with only O(log n) (arbitrary-precision) arithmetic operations. This
section explains one way to do it.
-}
--Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)

    negate (Cons val tail)                  = Cons (-val) (negate tail)    

    (+) (Cons aVal aTail) (Cons bVal bTail) = Cons (aVal + bVal) (aTail + bTail)

    (*) (Cons aVal aTail) s@(Cons bVal bTail) 
        = Cons (aVal * bVal) (streamMap (* aVal) bTail + (aTail * s))

instance Fractional (Stream Integer) where
    (/) (Cons aVal aTail) (Cons bVal bTail) = divRes
      where 
        divRes = Cons (aVal `div` bVal) (streamMap (`div` bVal) (aTail - divRes * bTail))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)


{-
Exercise 7 (Optional)
• Create a type Matrix which represents 2 × 2 matrices of Integers.
• Make an instance of the Num type class for Matrix. In fact, you only
have to implement the (*) method, since that is the only one we
will use. (If you want to play around with matrix operations a bit
more, you can implement fromInteger, negate, and (+) as well.)
• We now get fast (logarithmic time) matrix exponentiation for free,
since (^) is implemented using a binary exponentiation algorithm
in terms of (*). Write a function
fib4 :: Integer -> Integer
which computes the nth Fibonacci number by raising F to the nth
power and projecting out Fn (you will also need a special case
for zero). Try computing the one millionth or even ten millionth
Fibonacci number. 
-}
--Exercise 7
data Matrix = Square Integer Integer Integer Integer

instance Num Matrix where
    (*) (Square a1 a2 a3 a4) (Square b1 b2 b3 b4) =
        Square (a1 * b1 + a2 * b3) (a1 * b2 + a2 * b4) 
            (a3 * b1 + a4 * b3) (a3 * b2 + a4 * b4)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = getNum $ rootMatrix^n
  where 
    rootMatrix = Square 1 1 1 0

    getNum :: Matrix -> Integer
    getNum (Square _ a2 _ _) = a2

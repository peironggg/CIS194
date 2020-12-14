{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

--Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 .. ]

--Exercise 2
fibs2 :: [Integer]
fibs2 = fibGen 0 1
  where
    fibGen :: Integer -> Integer -> [Integer]
    fibGen a b = a : fibGen b (a + b)

--Exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons val tail) = val : streamToList tail

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

--Exercise 4
streamRepeat :: a -> Stream a
streamRepeat val = Cons val (streamRepeat val)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap func (Cons val tail) = Cons (func val) (streamMap func tail)       

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed func val = Cons val (streamFromSeed func (func val))

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





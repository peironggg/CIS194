-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a,b)]
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

-- Exercise 6
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 a b _ _ = [(a,b)]
hanoi4 2 a b c _ = [(a,c), (a,b), (c,b)]
hanoi4 n a b c d = hanoi4 (n-2) a d b c ++ [(a,c), (a,c), (c,b)] ++ hanoi4 (n-2) d b a c

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
  | length (xs) `mod` 2 == 0 = doubleEven xs
  | otherwise                = doubleOdd xs

doubleEven :: [Integer] -> [Integer]
doubleEven [] = []
doubleEven (x:[]) = [2*x]
doubleEven (x:y:zs) = 2*x : y : doubleEven zs

doubleOdd :: [Integer] -> [Integer]
doubleOdd [] = []
doubleOdd (x:[]) = [x]
doubleOdd (x:y:zs) = x : 2*y : doubleOdd zs

-- Exercise 3
sumDigit :: Integer -> Integer
sumDigit n
  | n == 0    = 0
  | otherwise = n `mod` 10 + sumDigit (n `div` 10)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumDigit x + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

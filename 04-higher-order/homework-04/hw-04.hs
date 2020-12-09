import Data.List

--Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x   = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

--Exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where 
    insert elem Leaf = Node 0 Leaf elem Leaf
    insert elem (Node hgt left val right)
        | getHgt left > getHgt right = Node hgt                                        left val (insert elem right)
        | getHgt left < getHgt right = Node hgt                                        (insert elem left) val right
        | otherwise                  = Node (1 + (getMaxHgt right $ insert elem left)) (insert elem left) val right
    getHgt Leaf                    = -1
    getHgt (Node hgt _ _ _)        = hgt
    getMaxHgt firstTree secondTree = max (getHgt firstTree) (getHgt secondTree)

-- putStr . showTree $ foldTree "ABCDEFGHIJ" to print the tree
showTree Leaf = ""  
showTree n@(Node i _ _ _) = go i n
    where
        go _ (Leaf) = "" 
        go i (Node _ l c r) = go (i - 1) l ++ 
            replicate (4 * fromIntegral i) ' ' ++ show c ++ "\n" ++ go (i - 1) r 


--Exercise 3
xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f initial xs = foldr (flip f) initial (reverse xs)

--Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram =  map (\num -> num * 2 + 1) . removeUnwanted
  where 
    cartProd :: [a] -> [b] -> [(a, b)]
    cartProd xs ys = [(x,y) | x <- xs, y <- ys]
    -- build :: Integer -> [Integer]
    build n = [1 .. (n + 1)]
    -- findUnwanted :: [Int] -> [Int]
    findUnwanted xs = map (\(i, j) -> i + j + 2 * i * j) . filter (\(i, j) -> i + j + 2 * i * j <= length xs) . cartProd xs . xs
    -- removeUnwanted :: Integer -> [Integer]
    removeUnwanted n = build n

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
  where insert elem Leaf = Node 0 Leaf elem Leaf
        insert elem (Node hgt left val right)
          | getHgt left > getHgt right = Node hgt                    left val (insert elem right)
          | getHgt left < getHgt right = Node hgt                    (insert elem left) val right
          | otherwise                  = Node (getMaxHgt left right) (insert elem left) val right
        getHgt Leaf             = -1
        getHgt (Node hgt _ _ _) = hgt
        getMaxHgt left right = 1 + max (getHgt left) (getHgt right)

-- putStr . showTree $ foldTree "ABCDEFGHIJ" to print the tree
showTree Leaf = ""  
showTree n@(Node i _ _ _) = go i n
  where
  go _ (Leaf) = "" 
  go i (Node _ l c r) = go (i-1) l ++ 
    replicate (4*fromIntegral i) ' ' ++ show c ++ "\n" ++ go (i-1) r 
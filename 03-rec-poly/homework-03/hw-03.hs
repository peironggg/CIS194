--Exercise 1
skips :: [a] -> [[a]]
skips xs = map (\(idx, _) -> filterIdx idx ps) ps
  where ps             = zip [1 .. length xs] xs
        filterIdx n ls = map snd $ filter (\(idx, _) -> idx `mod` n == 0) ls

--Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : zs)
  | y > x && z > y = y : localMaxima (z : zs)
  | otherwise      = localMaxima (y : z : zs)

--Exercise 3
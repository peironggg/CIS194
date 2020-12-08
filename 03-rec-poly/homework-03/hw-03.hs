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
histogram :: [Integer] -> String
histogram xs = unlines (map (\row -> genLine freqMap row) [mostFreq, mostFreq - 1 .. 1]) ++ "==========\n0123456789\n"
  where getFreq ls          = map (\num -> length $ filter (== num) ls) [0 .. 9]
        freqMap             = getFreq xs
        mostFreq            = maximum freqMap
        genLine freqMap row = map (\freq -> getStr freq row) freqMap
        getStr freq row
          | freq >= row     = '*'
          | otherwise       = ' '

import Data.Maybe

--Exercise 1
skips :: [a] -> [[a]]
skips xs = filter (catMaybes (zipWith (filterIdx) (take (length xs) (repeat (zip [1..] xs))) [1..]))
  where 
filterIdx (curr, elem) idx
    | (curr `mod` idx) == 0  = Just elem
    | otherwise              = Nothing





    -- [[(1,'a'), (2, 'b'), (3, 'c')],
    --  [(1,'a'), (2, 'b'), (3, 'c')]]   [1, 2]
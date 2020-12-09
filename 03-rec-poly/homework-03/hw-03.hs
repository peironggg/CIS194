{-# LANGUAGE ScopedTypeVariables #-}

{-
Exercise 1 Hopscotch
Your first task is to write a function
cis 194: homework 3 2
skips :: [a] -> [[a]]
The output of skips is a list of lists. The first list in the output should be the same as the input list. The second list in the output should contain every second element from the input list. . . and the nth list in the output should contain every nth element from the input list.
For example:
== ["ABCD", "BD", "C", "D"]
== ["hello!", "el!", "l!", "l", "o", "!"]
== [[1]]
skips "ABCD"
skips "hello!"
skips [1]
skips [True,False] == [[True,False], [False]]
skips []           == []
Note that the output should be the same length as the input.
-}
--Exercise 1
skips :: forall a. [a] -> [[a]]
skips xs = map (\(idx, _) -> filterIdx idx ps) ps
  where
    ps :: [(Int, a)]
    ps = zip [1 .. length xs] xs

    filterIdx :: Int -> [(Int, a)] -> [a]
    filterIdx n ls = map snd $ filter (\(idx, _) -> idx `mod` n == 0) ls


{-
Exercise 2 Local maxima
A local maximum of a list is an element of the list which is strictly
greater than both the elements immediately before and after it. For example, in the list [2,3,4,1,5], the only local maximum is 4, since it is greater than the elements immediately before and after it (3 and 1). 5 is not a local maximum since there is no element that comes after it.
Write a function
localMaxima :: [Integer] -> [Integer]
which finds all the local maxima in the input list and returns them in order. For example:
 localMaxima [2,9,5,6,1] == [9,6]
 localMaxima [2,3,4,1,5] == [4]
 localMaxima [1,2,3,4,5] == []
-}
--Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : zs)
    | y > x && z > y = y : localMaxima (z : zs)
    | otherwise      = localMaxima (y : z : zs)
localMaxima _ = []


{-
Exercise 3 Histogram
For this task, write a function
histogram :: [Integer] -> String
which takes as input a list of Integers between 0 and 9 (inclusive), and outputs a vertical histogram showing how many of each number were in the input list. You may assume that the input list does not contain any numbers less than zero or greater than 9 (that is, it does not matter what your function does if the input does contain such numbers). Your output must exactly match the output shown in the examples below.
cis 194: homework 3 3
histogram [1,1,1,5] ==
*
* **
 ==========
 0123456789
 histogram [1,4,5,4,6,6,3,4,2,4,9] ==
*
* **
  ******  *
 ==========
 0123456789
Important note: If you type something like histogram [3,5] at the ghci prompt, you should see something like this:
 "   * *    \n==========\n0123456789\n"
This is a textual representation of the String output, including \n escape sequences to indicate newline characters. To actually visualize the histogram as in the examples above, use putStr, for example, putStr (histogram [3,5]).
-}
--Exercise 3
histogram :: [Integer] -> String
histogram xs = unlines (map (\row -> genLine freqMap row) [mostFreq, mostFreq - 1 .. 1]) ++ "==========\n0123456789\n"
  where
    getFreq :: [Integer] -> [Int] 
    getFreq ls = map (\num -> length $ filter (== num) ls) [0 .. 9]

    freqMap :: [Int]
    freqMap = getFreq xs

    mostFreq :: Int
    mostFreq = maximum freqMap

    genLine :: [Int] -> Int -> String
    genLine freqMap row = map (\freq -> getStr freq row) freqMap

    getStr :: Int -> Int -> Char
    getStr freq row
        | freq >= row = '*'
        | otherwise   = ' '
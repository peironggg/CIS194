{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Char
import Data.Foldable (foldl')
import Data.Monoid

{-
Exercise 3 Mr. Dickens’s publishing company has changed their
minds. Instead of paying him by the word, they have decided to pay him according to the scoring metric used by the immensely popular
game of Scrabble . You must therefore update your editor imple- mentation to count Scrabble scores rather than counting words.

Hence, the second annotation you decide to implement is one to cache the ScrabbleTM score for every line in a buffer. 
Create a Scrabble module that defines a Score type, a Monoid instance for Score, and the following functions:
score :: Char -> Score scoreString :: String -> Score
The score function should implement the tile scoring values as shown at http://www.thepixiepit.co.uk/scrabble/rules.html; any characters not mentioned (punctuation, spaces, etc.) should be given zero points.
To test that you have everything working, add the line import Scrabble to the import section of your JoinList module, and write the follow-
ing function to test out JoinLists annotated with scores:
scoreLine :: String -> JoinList Score String
Example:
 *JoinList> scoreLine "yay " +++ scoreLine "haskell!"
 Append (Score 23)
        (Single (Score 9) "yay ")
        (Single (Score 14) "haskell!")
-}
--Exercise 3
newtype Score = Score { getScore :: Int } 
    deriving (Eq, Ord, Show, Num)

class Scored a where
    scoreAttr :: a -> Score

instance Scored Score where
    scoreAttr = id

instance Scored a => Scored (a, b) where
    scoreAttr = scoreAttr . fst

instance Monoid Score where
    mempty  = Score 0    

instance Semigroup Score where
    (<>) = (+)

--score function copied as url to Scrabble rules no longer working
score :: Char -> Score
score c
    | c' `elem` "aeilnorstu" = Score 1
    | c' `elem` "dg"         = Score 2
    | c' `elem` "bcmp"       = Score 3
    | c' `elem` "fhvwy"      = Score 4
    | c' `elem` "k"          = Score 5
    | c' `elem` "jx"         = Score 8
    | c' `elem` "qz"         = Score 10
    | otherwise              = Score 0
      where
        c' :: Char
        c' = toLower c

scoreString :: String -> Score
scoreString = foldl' (\accumScore curr -> accumScore + score curr) (Score 0)
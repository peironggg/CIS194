{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Char
import Data.Foldable (foldl')
import Data.Monoid

--Exercise 4
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
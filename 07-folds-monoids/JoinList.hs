{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Data.Foldable (foldl')
import Buffer
import Editor
import Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

type DList a = [a] -> [a]

toDList :: [a] -> DList a
toDList xs  = (xs ++ )

fromDList :: DList a -> [a]
fromDList f = f []

jlToDList :: JoinList m a -> DList a
jlToDList Empty              = toDList []
jlToDList (Single _ a)       = toDList [a]
jlToDList (Append _ jl1 jl2) = jlToDList jl1 . jlToDList jl2

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

--Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2
    

--Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n (Single _ a)
    | n == 0    = Just a
    | otherwise = Nothing
indexJ n (Append m jl1 jl2)    
    | n < jl1Size = indexJ n             jl1
    | otherwise   = indexJ (n - jl1Size) jl2
      where
        jl1Size :: Int
        jl1Size = getSize . size . tag $ jl1
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n (Single m a)
    | n >= 1    = Empty
    | otherwise = Single m a
dropJ n (Append _ jl1 jl2)
    | n < jl1Size = dropJ n             jl1 +++ jl2
    | otherwise   = dropJ (n - jl1Size) jl2
      where
        jl1Size :: Int
        jl1Size = getSize . size . tag $ jl1
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n (Single m a)
    | n <= 0    = Empty
    | otherwise = Single m a
takeJ n (Append _ jl1 jl2)
    | n < jl1Size = takeJ n jl1
    | otherwise   = jl1 +++ takeJ (n - jl1Size) jl2
      where
        jl1Size :: Int
        jl1Size = getSize . size . tag $ jl1
takeJ _ _ = Empty

--Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

--Exercise 4
instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . fromDList . jlToDList

    fromString = foldl' func Empty . lines
      where
        func :: JoinList (Score, Size) String -> String -> JoinList (Score, Size) String
        func jl str = jl +++ Single (scoreString str, 1) str

    line = indexJ

    replaceLine n str jl = takeJ n jl +++ fromString str +++ dropJ (n + 1) jl

    numLines = getSize . size . tag

    value = getScore . scoreAttr . tag

--Running editor
main = runEditor editor (Single (Score 0, Size 0) [] :: JoinList (Score, Size) String)



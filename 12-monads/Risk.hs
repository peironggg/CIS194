{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

--Exercise 1
battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield att def) = do
    attDice <- sortedDiceVals maxAtt
    defDice <- sortedDiceVals maxDef
    let didWin = zipWith (>) attDice defDice

    return $ foldr updateBf bf didWin
      where
        maxAtt :: Int
        maxAtt = min 3 (att - 1)

        maxDef :: Int
        maxDef = min 2 def

        sortedDiceVals :: Int -> Rand StdGen [Int]
        sortedDiceVals n = map unDV . reverse . sort <$> replicateM n die

        updateBf :: Bool -> Battlefield -> Battlefield
        updateBf win (Battlefield att def)
            | win       = Battlefield att (def - 1)
            | otherwise = Battlefield (att - 1) def

--Exercise 2
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield att def)
    | att < 2 || def <= 0 = return bf
    | otherwise           = battle bf >>= invade

--Exercise 3
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
    res <- replicateM 1000 (invade bf)
    let winCount = length $ filter id (fmap isWin res)
    return $ fromIntegral winCount / 1000
      where
        isWin :: Battlefield -> Bool
        isWin (Battlefield att def) = att > def

exactSuccessProb :: Battlefield -> Double
exactSuccessProb bf = evalRand (successProb bf) (mkStdGen 1)
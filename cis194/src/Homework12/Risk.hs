{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework12.Risk where

import Control.Monad
import Control.Monad.Random
import Data.List (sort)

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

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Exercise 2

getSoldiers :: Battlefield -> (Army, Army)
getSoldiers (Battlefield totalAtt totalDef) = (att, def)
          where
             att = if totalAtt > 3 then 3 else totalAtt - 1
             def = if totalDef > 2 then 2 else totalDef 
               
generateBattlefiled :: (Army, Army) -> Battlefield
generateBattlefiled (att, def) = Battlefield att def               
               
simulateBattle :: ([DieValue], [DieValue]) -> (Army, Army) -> (Army, Army)
simulateBattle ([] , _ ) armies = armies
simulateBattle (_  , []) armies = armies
simulateBattle (a:as, d:ds) (att, def)
  | a > d     = simulateBattle (as, ds) (att, def - 1)
  | otherwise = simulateBattle (as, ds) (att - 1, def)
               
battleOutcome :: (Army, Army) -> ([DieValue], [DieValue]) -> Battlefield
battleOutcome armies (attRolls, defRolls) = generateBattlefiled $ simulateBattle sortedRolls armies
            where sortedRolls = (sort attRolls, sort defRolls)  

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = dice (att + def) >>= \ds -> return $ battleOutcome (att, def) (splitAt att ds)  
      where 
        (att, def) = getSoldiers bf  
        
-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | attackers bf < 2 || defenders bf < 1 = return bf
  | otherwise                            = battle bf >>= invade
  
-- Exercise 4

hasAttackSucceeded :: Battlefield -> Bool
hasAttackSucceeded = (==0) . defenders

success :: [Battlefield] -> Rand StdGen Double
success bfs = return $ fromIntegral (length attVictory) / fromIntegral (length bfs)
  where attVictory = filter hasAttackSucceeded bfs

successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 1000 (invade bf) >>= success

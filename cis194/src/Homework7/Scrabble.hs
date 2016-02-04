{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Scrabble where

import qualified Data.Map.Strict as Map

-- Exercise 3

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

getScore :: Score -> Int
getScore (Score a) = a

scoreMap :: Map.Map Char Score
scoreMap = Map.fromList [('a', 1), ('b', 3), ('c', 3), ('d', 2), ('e', 1),
                         ('f', 4), ('g', 2), ('h', 4), ('i', 1), ('j', 8),
                         ('k', 5), ('l', 1), ('m', 3), ('n', 1), ('o', 1),
                         ('p', 3), ('q', 10), ('r', 1), ('s', 1), ('t', 1),
                         ('u', 1), ('v', 4), ('w', 4), ('x', 8), ('y', 4),
                         ('z', 10)]

score :: Char -> Score
score c = case Map.lookup c scoreMap of
                (Just n) -> n
                Nothing  -> Score 0

scoreString :: String -> Score
scoreString = foldr ((+) . score) 0

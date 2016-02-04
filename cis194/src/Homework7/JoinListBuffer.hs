{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JoinListBuffer where

import           Data.Monoid
import           JoinList
import           Scrabble
import           Sized

import           Buffer

-- Exercise 4

type JoinListBuffer = (JoinList (Score, Size) String)

instance Buffer (JoinList (Score, Size) String) where
    toString    = unlines . jlToList
    fromString  = foldr ( (+++) . stringToJl ) Empty . lines
                    where stringToJl s = elemToJl (scoreString s, 1) s
    line        = indexJ
    replaceLine i s jlb
                | i < 0             = jlb
                | i > numLines jlb  = jlb
                | otherwise         = takeJ i jlb +++ fromString s +++ dropJ (i+1) jlb
    numLines    = getSize  . snd . tag
    value       = getScore . fst . tag

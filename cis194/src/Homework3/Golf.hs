{-# OPTIONS_GHC -Wall #-}

module Golf where

import           Data.List

-- Exercise 1 Hopscotch

type IndexPair a = (a, Int)

isNthElem :: Int -> IndexPair a -> Bool
isNthElem n (_, i) = i `mod` n == 0

getElement :: IndexPair a -> a
getElement (a, _) = a

nthElements :: Int -> [a] -> [a]
nthElements n xs = map getElement ( filter (isNthElem n) is )
                                where
                                    m = length xs
                                    is = zip xs [1..m]

skips :: [a] -> [[a]]
skips xs = [ nthElements n xs | n <- [1..m] ]
                        where m = length xs

-- Exercise 2 Local Maxima

nextIsLarger :: Ord a => (a, a) -> Bool
nextIsLarger (x, y) = x <= y

zipSpecial :: [a] -> [a] -> [(a, a)]
zipSpecial (x : xs) []         = [(x, x)]
zipSpecial [] (y : ys)         = [(y, y)]
zipSpecial (x : xs) (y : ys) = (x, y) : zipSpecial xs ys

reachLocalMaximum :: [Integer] -> [Integer]
reachLocalMaximum [] = []
reachLocalMaximum xxs@(x : xs) = map fst (dropWhile nextIsLarger pairs)
                                                where pairs = zipSpecial xxs xs

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima xs = case reachLocalMaximum xs of
                                    []           -> []
                                    (y : ys)   -> y : localMaxima ys

-- Exercise 3 Histogram

fillZero :: Int -> [Int] -> [Int]
fillZero x []
              | x < 9 = 0 : fillZero (x+1) []
              | otherwise = []
fillZero x yys@(y : ys)
                    | x + 1 == y   = frequency yys
                    | otherwise     = 0 : fillZero (x+1) yys

-- Assumes the list of integers is ordered
frequency :: [Int] -> [Int]
frequency [] = []
frequency xxs@(x : xs) = n : fillZero x (drop n xxs)
                                                where
                                                    n = length (takeWhile (==x) xxs)

addStar :: Int -> Int -> Char
addStar f m
              | m < f       = ' '
              | otherwise = '*'

buildRows :: Int -> [Int] -> [String]
buildRows 0 xs = []
buildRows n xs = map (addStar n) xs : buildRows (n - 1) xs

buildHistogram :: [String] -> String
buildHistogram []         = replicate 10 '=' ++ "\n" ++ "0123456789\n"
buildHistogram (s : ss) = s ++ "\n" ++ buildHistogram ss

histogram :: [Int] -> String
histogram xs = buildHistogram ss
                        where
                            fs = fillZero (-1) (sort xs)
                            max = maximum fs
                            ss = buildRows max fs


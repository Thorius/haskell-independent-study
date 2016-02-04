{-# OPTIONS_GHC -Wall #-}
module HigherOrder where

import           Data.List

-- Exercise 1 Wholemeal Programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =  sum . filter even . takeWhile (/=1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1 )

-- Exercise 2 Folding with Trees

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
        deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight Leaf = 0
treeHeight (Node h _ _ _) = h

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

foldTree :: [a] -> Tree a
foldTree []  = Leaf
foldTree [x] = Node 0 Leaf x Leaf
foldTree [x, y] = Node 1 (foldTree [x]) y Leaf
foldTree (x : xs) = Node (1 + ch) left x right
                    where
                        (ys, zs) = halve xs
                        left  = foldTree ys
                        right = foldTree zs
                        ch = max (treeHeight left) (treeHeight right)

-- Exercise 3 More Folds

xor :: [Bool] -> Bool
xor = foldl ( \p q ->  ( p || q) && not ( p && q ) ) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ( \x acc -> f x : acc ) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr ( \b g a -> g (f a b) ) id xs base

-- Exercise 4 Finding Primes

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [ (x, y) | x <- xs, y <- ys ]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) . filter (`notElem` crossed) $ [1..n]
                    where
                        crossed = map (\(i, j) -> i + j + 2*i*j) $ cartProd [1..n] [1..n]

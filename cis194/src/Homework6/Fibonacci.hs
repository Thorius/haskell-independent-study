{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where


-- Exercise 1

fib :: Integer -> Integer
fib n
    | n > 1  = fib (n-1) + fib (n-2)
    | n == 1 = 1
    | n == 0 = 0
    | otherwise = error "Cannot compute negative fibonacci number."

fibsRecursive :: [Integer]
fibsRecursive = map fib [0..]

-- Exercise 2

fibsDynamic :: [Integer]
fibsDynamic = 0 : 1 : zipWith (+) fibsDynamic (tail fibsDynamic)

-- Exercise 3

data Stream a = a :| Stream a

streamToList :: Stream a -> [a]
streamToList (a :| as) = a : streamToList as

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

intStream :: Stream Integer
intStream = foldr (:|) undefined [0..]

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat = foldr (:|) undefined . repeat

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (a :| as) = f a :| streamMap f as

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = s :| streamFromSeed f (f s)

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (a :| as) bs = a :| interleaveStreams bs as

ruler :: Stream Integer
ruler =  foldr interleaveStreams undefined (map streamRepeat [0, 1 .. ])

-- Exercise 6

x :: Stream Integer
x = 0 :| ( 1 :| streamRepeat 0)

instance Num (Stream Integer) where
    fromInteger n = n :| streamRepeat 0
    negate = streamMap negate
    (a :| as) + (b :| bs) = (a + b) :| (as + bs)
    (a :| as) * bbs@(b :| bs) = (a * b) :| ((streamMap (*a) bs) + (as * bbs))
    abs = streamMap abs
    signum = streamMap signum

instance Fractional (Stream Integer) where
    fromRational _ = nats
    aas@(a :| as) / bbs@(b :| bs) = (a `div` b) :| ( streamMap (`div` b) ( as - (aas / bbs) * bs) )

fibsStream :: Stream Integer
fibsStream = x / (1 - x - x*x)

-- Exercise 7

data Matrix = Matrix Integer Integer Integer Integer
    deriving Show

instance Num Matrix where
    fromInteger n = Matrix n 0 0 n
    negate (Matrix a b c d) = Matrix (-a) (-b) (-c) (-d)
    (Matrix a1 b1 c1 d1) + (Matrix a2 b2 c2 d2) = Matrix a3 b3 c3 d3
                                where
                                    a3 = a1 + a2
                                    b3 = b1 + b2
                                    c3 = c1 + c2
                                    d3 = d1 + d2
    (Matrix a1 b1 c1 d1) * (Matrix a2 b2 c2 d2) = Matrix a3 b3 c3 d3
                                where
                                    a3 = a1 * a2 + b1 * c2
                                    b3 = a1 * b2 + b1 * d2
                                    c3 = c1 * a2 + d1 * c2
                                    d3 = c1 * b2 + d1 * d2
    abs (Matrix a b c d) = Matrix (abs a) (abs b) (abs c) (abs d)
    signum (Matrix a b c d) = Matrix (signum a) (signum b) (signum c) (signum d)

projectMatrix :: Matrix -> Integer
projectMatrix (Matrix _ b _ _) = b

fibsMatrix :: Integer -> Integer
fibsMatrix 0 = 0
fibsMatrix n = projectMatrix $ fibM ^ n
                    where fibM = Matrix 1 1 1 0

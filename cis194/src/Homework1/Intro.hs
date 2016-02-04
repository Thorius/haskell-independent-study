{-# OPTIONS_GHC -Wall #-}

-- Credit Card Validation

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits n
            | n <= 0 = []
            | otherwise = toDigits( n `div` 10) ++ [ n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x1 : x2 : xs) = x1 : (2*x2) : doubleEveryOther xs

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
                | x < 10     = x + sumDigits xs
                | otherwise = (x `div` 10) + (x `mod` 10) + sumDigits xs

-- Exercise 4

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigitsRev n) ) `mod` 10 == 0

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)

hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 1 a b _ = [(a, b)]
hanoi3 n a b c = hanoi3 (n-1) a c b ++ [(a, b)] ++ hanoi3 (n-1) c b a

-- Exercise 6 (Optional)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]

hanoi4 1 a b _ _ = [(a, b)]
hanoi4 n a b c d = hanoi4 k a c b d ++ hanoi3 (n-k) a b d ++ hanoi4 k c b a d
                            where k = n `quot` 2 --round( sqrt(2 * (toDouble n) +1) ) - 1

-- Helper functions

toDouble :: Integer -> Double
toDouble = fromIntegral


module JoinList where

import           Data.Monoid
import           Scrabble
import           Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
     deriving (Eq,Show)

-- Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jll +++ jlr = Append (tag jll `mappend` tag jlr) jll jlr

-- Exercise 2

(!!?) :: [a] -> Int -> Maybe a
[]      !!? _           = Nothing
_       !!? i | i < 0   = Nothing
(x:xs)  !!? 0           = Just x
(x:xs)  !!? i           = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty              = []
jlToList (Single _ a)       = [a]
jlToList (Append _ l1 l2)   = jlToList l1 ++ jlToList l2

elemToJl :: Monoid m => m -> a -> JoinList m a
elemToJl = Single

mSize :: Sized a => a -> Int
mSize = getSize . size

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
                    | i == 0    = Just a
                    | otherwise = Nothing
indexJ i (Append _ l1 l2)
                    | i < 0         = Nothing
                    | i < leftSize  = indexJ i l1
                    | otherwise     = indexJ (i - leftSize) l2
            where leftSize = mSize $ tag l1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty           = Empty
dropJ 0 jl              = jl
dropJ n (Single _ a)    = Empty
dropJ n (Append m l1 l2)
                    | n > mSize m  = Empty
                    | n > leftSize = dropJ (n - leftSize) l2
                    | otherwise    = dropJ n l1 +++ l2
            where leftSize = mSize $ tag l1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n lj@(Single _ a)
                    | n <= 0    = Empty
                    | otherwise = lj
takeJ n lj@(Append m l1 l2)
                    | n <= 0       = Empty
                    | n >= mSize m = lj
                    | n >= leftSize = l1 +++ takeJ (n - leftSize) l2
                    | otherwise    = takeJ n l1
            where leftSize = mSize $ tag l1

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s





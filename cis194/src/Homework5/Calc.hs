{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Calc where

import qualified Data.Map as M
import           ExprT
import           Parser
import           StackVM

-- Exercise 1

eval :: ExprT -> Integer
eval (LitT n) = n
eval (AddT x y) = eval x + eval y
eval (MulT x y) = eval x * eval y

-- Exercise 2

evalM :: Maybe ExprT -> Maybe Integer
evalM (Just e) = Just (eval e)
evalM Nothing = Nothing

evalStr :: String -> Maybe Integer
evalStr = evalM . parseExp LitT AddT MulT

-- Exercise 3

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = LitT
    add = AddT
    mul = MulT

reify :: ExprT -> ExprT
reify = id

-- Exercise 4

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)


instance Expr Bool where
    lit n
        | n <= 0    = False
        | otherwise = True
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 ( (x + y) `mod` 7 )
    mul (Mod7 x) (Mod7 y) = Mod7 ( (x * y) `mod` 7 )

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp
testBool    :: Maybe Bool
testBool    = testExp
testMM      :: Maybe MinMax
testMM      = testExp
testSat     :: Maybe Mod7
testSat     = testExp

-- Exercise 5

instance Expr Program where
    lit n   = [ PushI n ]
    add x y = x ++ y ++ [ Add ]
    mul x y = x ++ y ++ [ Mul ]

compile :: String -> Maybe Program
compile = parseExp lit add mul

evalProgram :: Maybe Program -> Either String StackVal
evalProgram (Just p) = stackVM p
evalProgram Nothing  = Left "Invalid compilation."

-- Exercise 6

class HasVars a where
    var :: String -> a

data VarExprT = LitV Integer
              | AddV VarExprT VarExprT
              | MulV VarExprT VarExprT
              | VarV String
    deriving (Show, Eq)

instance Expr VarExprT where
    lit = LitV
    add = AddV
    mul = MulV

instance HasVars VarExprT where
    var = VarV

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n   _ = Just n
    add x y m = case (x m, y m) of
                (Just i, Just j) -> Just (i + j)
                (_, _)           -> Nothing
    mul x y m = case (x m, y m) of
                (Just i, Just j) -> Just (i * j)
                (_, _)           -> Nothing

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs expr = expr $ M.fromList vs

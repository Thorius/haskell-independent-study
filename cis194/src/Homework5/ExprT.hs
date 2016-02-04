module ExprT where

data ExprT = LitT Integer
           | AddT ExprT ExprT
           | MulT ExprT ExprT
  deriving (Show, Eq)

-- | Data types for a simple language of arithmetic expressions.

module Arith where

data Exp = Const Integer |
           BinOpCall BinOp Exp Exp deriving (Eq, Ord, Show)

data BinOp = Add | Sub | Mul | Div deriving (Eq, Ord, Show)


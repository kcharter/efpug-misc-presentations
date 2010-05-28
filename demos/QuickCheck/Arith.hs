-- | Data types for a simple language of arithmetic expressions.

module Arith where

data Exp = Const Integer |
           BinOpCall BinOp Exp Exp deriving (Eq, Ord, Show)

data BinOp = Add | Sub | Mul | Div deriving (Eq, Ord, Show)

class ToExp a where
    toExp :: a -> Exp

instance ToExp Exp where
    toExp = id

instance ToExp Integer where
    toExp = Const

infixl 6 <+>, <->
infixl 7 <*>, </>

(<+>) :: (ToExp a, ToExp b) => a -> b -> Exp
(<+>) = libBinOp Add

(<->) :: (ToExp a, ToExp b) => a -> b -> Exp
(<->) = libBinOp Sub

(<*>) :: (ToExp a, ToExp b) => a -> b -> Exp
(<*>) = libBinOp Mul

(</>) :: (ToExp a, ToExp b) => a -> b -> Exp
(</>) = libBinOp Div

libBinOp :: (ToExp a, ToExp b) => BinOp -> a -> b -> Exp
libBinOp = liberalize2 . BinOpCall

liberalize2 :: (ToExp a, ToExp b) => (Exp -> Exp -> Exp) -> a -> b -> Exp
liberalize2 f x y = f (toExp x) (toExp y)
-- | A parser for arithmetic expressions involving unary minus, the
-- four binary operators, and parentheses.
module Exps (parseExp) where

import Control.Monad (liftM)
import Data.List (foldl')
import Text.ParserCombinators.Parsec

import Util

-- | A utility function that returns either an error message or an integer.
parseExp :: String -> Either String Integer
parseExp =
  either (Left . show) Right . runParser (topLevel pExp) () ""

-- | Parse a general expression.
pExp :: CharParser s Integer
pExp = pExp2

-- | Addition and subtraction.
pExp2 :: CharParser s Integer
pExp2 = pExp1 `applyOps` many (addSome <|> subSome)
  where addSome = opBy opPlus (+) pExp1
        subSome = opBy opMinus (-) pExp1
        
-- | Multiplication and division.
pExp1 :: CharParser s Integer
pExp1 = pExp0 `applyOps` many (mulBy <|> divBy)
  where mulBy = opBy opStar (*) pExp0
        divBy = opBy opSlash div pExp0

-- | A unary minus expression, or a literal
pExp0 :: CharParser s Integer
pExp0 = (opMinus >> negate `liftM` pExp0) <|> pNat <|> parens pExp

-- | Parse a natural number
pNat :: CharParser s Integer
pNat = read `liftM` lexeme (many1 digit)

-- | Applies an optional tail of operations to an initial value
applyOps :: CharParser s a -> CharParser s [a -> a] -> CharParser s a
applyOps first ops = first >>= \i -> foldl' (flip ($)) i `liftM` ops

-- | Takes a parser for a binary operator, its interpretation
-- function, and a parser for its right operand, and returns a parser
-- for an operation "tail".
opBy :: CharParser s x -> (a -> a -> a) -> CharParser s a -> CharParser s (a -> a)
opBy pOp f p = pOp >> flip f `liftM` p

opPlus  = sym '+'
opMinus = sym '-'
opStar  = sym '*'
opSlash = sym '/'

parens p = do sym '('; r <- p; sym ')'; return r

sym = lexeme . char


-- | Here's one of the simplest things you can use parsec for: parsing
-- an integer literal. This is useful in situations where you'd like
-- to avoid an @error@ that bounces you out of your program if there
-- is a syntax error.

module Integers (parseInteger) where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec

import Util

-- | A utility function that returns either an error message or an integer.
parseInteger :: String -> Either String Integer
parseInteger =
  either (Left . show) Right . runParser (topLevel pInteger) () ""
  
-- | The main integer parser itself. Note that since parsers are
-- monadic, we can use 'liftM'.
pInteger :: CharParser s Integer
pInteger = read `liftM` lexeme (many1 digit)


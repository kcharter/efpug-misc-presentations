-- | Here's one of the simplest things you can use parsec for: parsing
-- an integer literal. This is useful in situations where you'd like
-- to avoid an @error@ that bounces you out of your program if there
-- is a syntax error.

module Integers (parseInteger) where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec

parseInteger :: String -> Either String Integer
parseInteger =
  either (Left . show) Right . runParser (topLevel pInteger) () ""
  
pInteger :: CharParser s Integer
pInteger = read `liftM` lexeme (many1 digit)

topLevel :: CharParser s a -> CharParser s a
topLevel p = do spaces; r <- p; eof; return r

lexeme :: CharParser s a -> CharParser s a
lexeme p = do r <- p; spaces; return r

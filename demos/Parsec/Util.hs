-- | Utility functions that we'll use in the examples.

module Util where

import Text.ParserCombinators.Parsec

-- | A utility that turns a lexeme parser into a parser that consumes
-- leading white space and ensures that all the input is parsed.
topLevel :: CharParser s a -> CharParser s a
topLevel p = do spaces; r <- p; eof; return r

-- | A utility that takes a character parser and returns an equivalent
-- parser that consumes trailing white space.
lexeme :: CharParser s a -> CharParser s a
lexeme p = do r <- p; spaces; return r

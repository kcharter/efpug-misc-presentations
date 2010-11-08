-- | Parsec has special support for parsing expressions with
-- operators. In the previous parsers, we've rolled our own support
-- for that. Here, we switch to using the expression parser support.
--
-- We're dealing with just four binary operators here, but we're able
-- to cut down on the code size quite nicely. Imagine if we had
-- boolean operators, relational operators, and bit-twiddling
-- operators in this language.

module ExpsUsingExpParser (parseExp) where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

-- | A utility function that returns either an error message or an integer.
parseExp :: String -> Either String Integer
parseExp =
  either (Left . show) Right . runParser (topLevel pExp) () ""

-- | Parse a general expression.
pExp :: CharParser s Integer
pExp = buildExpressionParser table pExp0
  where table = [[lop "*" (*), lop "/" div],
                 [lop "+" (+), lop "-" (-)]]
        lop opName f = Infix (sym opName >> return f) AssocLeft

-- | A unary minus expression, or a literal
pExp0 :: CharParser s Integer
pExp0 = (sym "-" >> negate `liftM` pExp0) <|> int <|> parens pExp

-- | The language definition for our expression language. Note that we
-- can have multi-line nested comments, single-line comments,
-- identifier characters, and so on. Think of this as a form that we
-- submit to the 'makeTokenParser' function.
--
-- Notice that we define the parsers for acceptable identifier letters
-- here. We don't use identifiers, so this isn't really necessary.
--
-- I've also avoided using the 'opStart' and 'opLetter' fields, and I
-- don't use the 'operator' or 'reservedOp' parsers in the token
-- parser. Every time I try using that stuff, I regret it. It seems to
-- be designed with Haskell in mind, where programmers can define
-- their own operators. In our case, the 'reservedOp' function has an
-- unfortunate clash with the unary minus operator. You would expect
-- the expression
--
--  1+-10
--
-- to be legal and evaluate to @-9@. However, instead you'll get a
-- parse error if you use 'reservedOp'. It will treat the @+-@ as an
-- operator name, and then complain that it was expecting only the
-- operator @+@. If you try to use 'operator' instead, it will happily
-- report that @+-@ is a single operator. This is consistent with the
-- way the 'reserved' and 'identifier' parsers work, but it's not
-- usually what you want.
--
-- If your language has a fixed set of binary operators, you are
-- better off just using 'symbol', as I do below.

langDef = emptyDef { commentStart = "/*",
                     commentEnd = "*/",
                     commentLine = "//",
                     nestedComments = True, 
                     identStart = letter <|> char '_',
                     identLetter = letter <|> digit <|> char '_',
                     caseSensitive = True }

-- | The token parser for our language. A token parser is a data type
-- whose fields are lexeme parsers for various kinds of basic
-- constructions, like numeric and string literals, punctation and so
-- on, and constructions delimited by different types of matching
-- parentheses. The 'whiteSpace' parser includes not just the usual
-- white space characters, but also comments, and each of the parsers
-- consumes trailing 'whiteSpace'.

tp = T.makeTokenParser langDef

int = T.integer tp
whiteSpace = T.whiteSpace tp
parens = T.parens tp
sym    = T.symbol tp

-- | We now need a different definition of the top-level utility, one
-- that consumes white space as defined by the token parser.
topLevel p = do whiteSpace; r <- p; eof; return r
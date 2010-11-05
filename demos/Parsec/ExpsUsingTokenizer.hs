-- | Here is another parser for arithmetic expressions. However,
-- unlike the roll-your-own approach to lexeme parsers that we took in
-- the 'Exps' module, here we use a Parsec language definition and a
-- token parser. Among other things, this lets us include comments,
-- and gets us the fundamental integer parser and a couple of others
-- for free. Otherwise, the parser here is almost unchanged from the
-- one in 'Exps'.

module ExpsUsingTokenizer (parseExp) where

import Control.Monad (liftM)
import Data.List (foldl')
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

-- | A utility function that returns either an error message or an integer.
parseExp :: String -> Either String Integer
parseExp =
  either (Left . show) Right . runParser (topLevel pExp) () ""

-- | Parse a general expression.
pExp :: CharParser s Integer
pExp = pExp2

-- | Multiplication and division.
pExp2 :: CharParser s Integer
pExp2 = pExp1 `applyOps` many (addSome <|> subSome)
  where addSome = opBy opPlus (+) pExp1
        subSome = opBy opMinus (-) pExp1
        
-- | Addition and subtraction.
pExp1 :: CharParser s Integer
pExp1 = pExp0 `applyOps` many (mulBy <|> divBy)
  where mulBy = opBy opStar (*) pExp0
        divBy = opBy opSlash div pExp0

-- | A unary minus expression, or a literal
pExp0 :: CharParser s Integer
pExp0 = (opMinus >> negate `liftM` pExp0) <|> int <|> parens pExp

-- | Applies an optional tail of operations to an initial value
applyOps :: CharParser s a -> CharParser s [a -> a] -> CharParser s a
applyOps first ops = first >>= \i -> foldl' (flip ($)) i `liftM` ops

-- | Takes a parser for a binary operator, its interpretation
-- function, and a parser for its right operand, and returns a parser
-- for an operation "tail".
opBy :: CharParser s x -> (a -> a -> a) -> CharParser s a -> CharParser s (a -> a)
opBy pOp f p = pOp >> flip f `liftM` p

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

opPlus  = sym "+"
opMinus = sym "-"
opStar  = sym "*"
opSlash = sym "/"

int = T.integer tp
whiteSpace = T.whiteSpace tp
parens = T.parens tp
sym    = T.symbol tp

-- | We now need a different definition of the top-level utility, one
-- that consumes white space as defined by the token parser.
topLevel p = do whiteSpace; r <- p; eof; return r
This demo gives you a whirlwind tour of Parsec's features with four
simple parsers and corresponding driver programs:

 Integers/ints

   parse an unsigned sequence of digits and return an Integer

 Exps/exps

   parse arithmetic expressions involving optionally signed integer
   constants, parenthesized expressions, and the four arithmetic
   binary operators for addition, subtraction, multiplication, and
   truncating division.

 ExpsUsingTokenizer/exps1

   extend the arithmetic language with the ability to ignore comments
   by switching to a Parsec token parser.

 ExpsUsingExpParser/exps2

   use a Parsec expression parser to simplify the way we parse
   expressions using the four binary operators

The stuff here uses features available in Parsec 2. Things to consider
for the future:

- switch to a hand-rolled lexer instead of using a Parsec token
  parser, and see what the performance benefit is with Criterion

- switch to Parsec 3 to show what the differences are

- make a hand-rolled lexer using Iteratees and try out iteratee-parsec

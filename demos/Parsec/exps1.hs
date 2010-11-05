-- | Driver program for the 'ExpsUsingTokenizer' parser.

module Main where

import System.Environment
import ExpsUsingTokenizer

main :: IO ()
main = mapM_ parseAndPrint =<< getArgs
  where parseAndPrint = either putStrLn print . parseExp

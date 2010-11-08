-- | Driver program for the 'ExpsUsingExpParser' parser.

module Main where

import System.Environment
import ExpsUsingExpParser

main :: IO ()
main = mapM_ parseAndPrint =<< getArgs
  where parseAndPrint = either putStrLn print . parseExp

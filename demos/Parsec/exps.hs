-- | Driver program for the 'Exps' parser.

module Main where

import System.Environment
import Exps

main :: IO ()
main = mapM_ parseAndPrint =<< getArgs
  where parseAndPrint = either putStrLn print . parseExp

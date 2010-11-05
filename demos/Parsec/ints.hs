-- | Driver for the 'Integers' parser.
module Main where

import System.Environment

import Integers

main :: IO ()
main = mapM_ parseAndPrint =<< getArgs
  where parseAndPrint = either putStrLn print . parseInteger

  
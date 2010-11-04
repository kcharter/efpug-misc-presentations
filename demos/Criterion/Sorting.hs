module Sorting (insertionSort, quickSort) where

import Data.List (partition)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort = doSort []
    where doSort sofar [] = sofar
          doSort sofar (x:rest) = doSort (insert x sofar) rest
          insert x [] = [x]
          insert x ys@(y:rest) | x <= y    = x:ys
                               | otherwise = y:(insert x rest)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort left ++ [x] ++ quickSort right
    where (left,right) = partition (x>) xs
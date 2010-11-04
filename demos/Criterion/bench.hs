module Main where

import Data.List (sort)
import System.Random

import Criterion
import Criterion.Main

import Sorting

main :: IO ()
main = oneBench

oneBench :: IO ()
oneBench =
    do list1000 <- rints 1000
       defaultMain [bench "insertionSort1000" (nf insertionSort list1000),
                    bench "quickSort1000" (nf quickSort list1000),
                    bench "sort1000" (nf sort list1000)]

allBenches :: IO ()
allBenches =
    do list3 <- rints 3
       list5 <- rints 5
       list10 <- rints 10
       list50 <- rints 50
       defaultMain [bench "insertionSort3" (nf insertionSort list3),
                    bench "quickSort3" (nf quickSort list3),
                    bench "insertionSort5" (nf insertionSort list5),
                    bench "quickSort5" (nf quickSort list5),
                    bench "insertionSort10" (nf insertionSort list10),
                    bench "quickSort10" (nf quickSort list10),
                    bench "insertionSort50" (nf insertionSort list50),
                    bench "quickSort50" (nf quickSort list50)]

rints :: Int -> IO [Int]
rints = randomList (0,999999)

randomList :: Random a => (a,a) -> Int -> IO [a]
randomList bounds n = sequence $ replicate n $ randomRIO bounds

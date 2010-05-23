module Main where

import Data.List
import qualified Data.Map as DM
import Test.QuickCheck

prop_SortPreservesLength xs = length (sort xs) == length xs

prop_SortPreservesElements xs = freqs (sort xs) == freqs (xs)

freqs :: Ord a => [a] -> DM.Map a Int
freqs = foldl' addElem DM.empty
    where addElem sofar x =
              maybe (incCount 0) incCount $ DM.lookup x sofar
              where incCount count = DM.insert x (1+count) sofar 

prop_SortSorts = sorted . sort 

sorted [] = True
sorted [_] = True
sorted (x:y:rest) = x <= y && sorted (y:rest)
   
data Foo = X | Y deriving (Eq, Ord, Show)

instance Arbitrary Foo where
    arbitrary = oneof [return X, return Y]
    shrink _ = []

allXs :: Gen Foo
allXs = return X



main = do quickCheckResult (prop_SortPreservesLength :: [Integer] -> Bool)
          quickCheck (prop_SortPreservesLength :: [Foo] -> Bool)
          quickCheck $ forAll (listOf allXs) prop_SortPreservesLength
          quickCheck (prop_SortPreservesElements :: [Integer] -> Bool)
          quickCheck (prop_SortSorts :: [Integer] -> Bool)



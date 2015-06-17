{-# LANGUAGE CPP #-}
module Main where

import HList

import Data.List

data Tree a = Node (Tree a) (Tree a) | Leaf a

{-# INLINE repR #-}
repR :: ([a] -> [a]) -> ([a] -> H a)
repR f = repH . f

{-# INLINE absR #-}
absR :: ([a] -> H a) -> ([a] -> [a])
absR g = absH . g

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (a:as) = qsort bs ++ [a] ++ qsort cs
               where
                  (bs , cs) = partition (< a) as

main :: IO ()
main = print (qsort [8,3,5,7,2,9,4,6,3,2])
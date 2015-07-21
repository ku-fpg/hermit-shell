{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude hiding (const)
import Data.List hiding ((\\))
import qualified Data.List as L

(\\) :: [Nat] -> [Nat] -> [Nat]
(\\) = (L.\\)

const :: a -> b -> a
const x = \y -> x

type Nat = Int

minfree :: [Nat] -> Nat
minfree xs = head ([0..] \\ xs)

minfree' :: [Nat] -> Nat -> Nat
minfree' xs b = head (const ([0..] \\ xs) b)

minfreeFast :: [Nat] -> Nat
minfreeFast xs = minfrom 0 (length xs, xs)

minfrom :: Nat -> (Nat, [Nat]) -> Nat
minfrom !a (!n, xs)
  | n == 0 = a
  | m == b - a = minfrom b (n - m, vs)
  | otherwise = minfrom a (m, us)
  where
    (us, vs) = partition (< b) xs
    b = a + 1 + (n `div` 2)
    m = length us

chooseB :: [Nat] -> Nat
chooseB xs = a + 1 + (n `div` 2)
  where
    a = 0
    n = length xs

descFrom :: Nat -> [Nat]
descFrom n = [n,n-1..0]

main :: IO ()
main = print (minfree (descFrom 1000))

{-# RULES "minfree-const-eqv" [~]
      forall xs b.
      minfree' xs b = minfree xs
  #-}

-- {-# RULES "minfrees-are-eqv" [~] forall xs b. minfreeFast xs = minfree' xs b
--   #-}

{-# RULES "diff-distr-over-++" [~]
      forall as bs cs. (as ++ bs) \\ cs = (as \\ cs) ++ (bs \\ cs)
  #-}

{-# RULES "++-elim" [~] forall as bs cs. as \\ (bs ++ cs) = (as \\ bs) \\ cs #-}

{-# RULES "diff-exchange" [~]
      forall as bs cs. (as \\ bs) \\ cs = (as \\ cs) \\ bs
  #-}

{-# RULES "diff-interchange" [~]
      forall as bs us vs. (as ++ bs) \\ (us ++ vs) = (as \\ us) ++ (bs \\ vs)
  #-}

{-# RULES "head-++" [~]
      forall xs ys. head (xs ++ ys) = if null xs then head ys else head xs
  #-}


{-# RULES "diff-partition" [~]
      forall (xs :: [Int]) (b :: Int).
                   const ([0::Int ..] \\ xs) b
                    =
                   let (us,vs) = partition (< b) xs
                   in
                   (([0..b-1] :: [Int]) \\ us) ++ ([b::Int ..] \\ vs)
  #-}
-- {-# RULES "diff-partition" [~]
--       forall xs b.
--         const ([0..] \\ xs) b
--          =
--         let (us, vs) = partition (<b) xs
--         in
--         ([0..b-1] \\ us) ++ ([b..] \\ vs)
--   #-}

{-# RULES "const-elim" [~]
    forall a b. const a b = a
  #-}


module Laws where

import Control.Applicative (pure, (<*>), Applicative)
import Control.Monad (liftM)
import Control.Monad.Reader
import Data.Functor.Identity
import Data.Map hiding (map)
import Data.Monoid ((<>), mempty, Monoid(..))

main :: IO ()
main = return ()

-- functor
{-# RULES "fmap-id"      [~]             fmap id = id #-}
{-# RULES "fmap-distrib" [~] forall g h. fmap (g.h) = fmap g . fmap h #-}

-- applicative
{-# RULES "identity"     [~] forall v.     pure id <*> v = v #-}
{-# RULES "homomorphism" [~] forall f x.   pure f <*> pure x = pure (f x) #-}
{-# RULES "interchange"  [~] forall u y.   u <*> pure y = pure ($ y) <*> u #-}
{-# RULES "composition"  [~] forall u v w. u <*> (v <*> w) = pure (.) <*> u <*> v <*> w #-}
{-# RULES "fmap-pure"    [~] forall g x.   pure g <*> x = fmap g x #-}

-- monad
{-# RULES "return-left"  [~] forall k x.   return x >>= k = k x #-}
{-# RULES "return-right" [~] forall k.     k >>= return = k #-}
{-# RULES "bind-assoc"   [~] forall j k l. (j >>= k) >>= l = j >>= (\x -> k x >>= l) #-}
-- As mentioned in the paper, we cannot define this as a GHC rewrite rule.
-- {-# RULES "fmap-liftm"   [~] forall f x.   liftM f x = fmap f x #-}

-- monoid
{-# RULES "mempty-left"  [~] forall x. mempty <> x = x #-}
{-# RULES "mempty-right" [~] forall x. x <> mempty = x #-}
{-# RULES "mappend-assoc" [~] forall x y z. (x <> y) <> z = x <> (y <> z) #-}

-- Since we can't unfold (++)
{-# RULES "append-fix" [~] (++) = myAppend #-}
myAppend :: [a] -> [a] -> [a]
myAppend []     ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

-- Since we can't unfold map
{-# RULES "map-fix" [~] Prelude.map = myMap #-}
myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

-- useful auxilliary lemmas
{-# RULES "append-right"   [~] forall x. x ++ [] = x #-}
{-# RULES "pure-singleton" [~] forall x. pure x = [x] #-}
{-# RULES "append-single-left" [~] forall x xs. [x] ++ xs = x:xs #-}
{-# RULES "bind-left-nil"  [~] forall k. [] >>= k = [] #-}
{-# RULES "bind-left-cons" [~] forall x xs k. (x:xs) >>= k = k x ++ (xs >>= k) #-}
{-# RULES "bind-append"    [~] forall m n k. (m >>= k) ++ (n >>= k) = (m ++ n) >>= k #-}
{-# RULES "foldr-id"       [~] Prelude.foldr (:) [] = id #-}
{-# RULES "append-undefined" [~] forall xs. undefined ++ xs = undefined #-}
{-# RULES "ap-left"        [~] forall xs. [] <*> xs = [] #-}
{-# RULES "ap-eval"        [~] forall f fs xs. (f:fs) <*> xs = map f xs ++ (fs <*> xs) #-}
{-# RULES "ap-map"         [~] forall f fs xs. map f (fs <*> xs) = map (f.) fs <*> xs #-}
{-# RULES "ap-append"      [~] forall xs ys zs. (xs <*> zs) ++ (ys <*> zs) = (xs ++ ys) <*> zs #-}

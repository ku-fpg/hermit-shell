{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Fold where

import Data.Aeson
import HERMIT.API.Types

-- | fold a definition
-- 
-- double :: Int -> Int
-- double x = x + x
-- 
-- 5 + 5 + 6
-- any-bu (fold 'double)
-- double 5 + 6
-- 
-- Note: due to associativity, if you wanted to fold 5 + 6 + 6,
-- you first need to apply an associativity rewrite.
fold :: Name -> Rewrite LCore
fold nm = Transform $ method "fold" [toJSON nm]

{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Unsafe where

import HERMIT.API.Types
import Data.Aeson

-- | replace the currently focused expression with a new expression
--   DOES NOT ensure that free variables in the replacement expression are in scope
unsafeReplace :: String -> Rewrite LCore
unsafeReplace s = Transform $ method "unsafeReplace" [toJSON s]


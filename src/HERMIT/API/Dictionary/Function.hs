{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Function where

import Data.Aeson
import HERMIT.API.Types

-- | perform the static argument transformation on a recursive function.
staticArg :: Rewrite LCore
staticArg = Transform $ method "staticArg" []

-- | perform the static argument transformation on a recursive function, only transforming type arguments.
staticArgTypes :: Rewrite LCore
staticArgTypes = Transform $ method "staticArgTypes" []

-- -- | perform the static argument transformation on a recursive function, only transforming the arguments specified (by index).
-- staticArgPos :: [Int] -> Rewrite LCore
-- staticArgPos ints = Transform $ method "staticArgPos" [toJSON ints]

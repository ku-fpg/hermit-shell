{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.FixPoint where

import Data.Aeson
import HERMIT.API.Types

-- | rewrite a function binding into a non-recursive binding using fix
fixIntro :: Rewrite LCore
fixIntro = Transform $ method "fixIntro" []

-- | Fixed-Point Computation Rule
-- fix t f  <==>  f (fix t f)
fixComputationRule :: BiRewrite LCore
fixComputationRule = BiTransform $ method "fixComputationRule" []


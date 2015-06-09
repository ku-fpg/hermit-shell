{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Reasoning where

import HERMIT.API.Types

import Data.Aeson

retraction :: String -> String -> Rewrite LCore -> BiRewrite LCore
retraction f g r
  = BiTransform $ method "retraction"
                         [ toJSON f
                         , toJSON g
                         , toJSON r
                         ]

retractionUnsafe :: String -> String -> BiRewrite LCore
retractionUnsafe f g
  = BiTransform $ method "retractionUnsafe"
                         [ toJSON f
                         , toJSON g
                         ]

unshadowQuantified :: Rewrite LCore
unshadowQuantified = Transform $ method "unshadowQuantified" []

mergeQuantifiers :: HermitName -> HermitName -> Rewrite LCore
mergeQuantifiers n1 n2
  = Transform $ method "mergeQuantifiers"
                       [ toJSON n1
                       , toJSON n2
                       ]



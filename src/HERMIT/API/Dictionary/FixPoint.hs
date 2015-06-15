{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.FixPoint where

import Data.Aeson
import HERMIT.API.Types

-- | rewrite a function binding into a non-recursive binding using fix
fixIntro :: Rewrite LCore
fixIntro = Transform $ method "fixIntro" []

{-| 
  Fixed-Point Computation Rule
  fix t f  <==>  f (fix t f)
-}
fixComputationRule :: BiRewrite LCore
fixComputationRule = BiTransform $ method "fixComputationRule" []

{-| 
  Rolling Rule
  fix tyA (\\ a -> f (g a))  <==>  f (fix tyB (\\ b -> g (f b))
-}
fixRollingRule :: BiRewrite LCore
fixRollingRule = BiTransform $ method "fixRollingRule" []

{-| 
  Fixed-point Fusion Rule
  Given f :: A -> B, g :: A -> A, h :: B -> B, and proofs that, for some x, 
  (f (g a) ==> x) and (h (f a) ==> x) and that f is strict, then
  f (fix g) <==> fix h
-}
fixFusionRule :: String -> String -> String
              -> Rewrite LCore -> Rewrite LCore
              -> Rewrite LCore -> BiRewrite LCore
fixFusionRule f g h r1 r2 strictf = BiTransform $ method "fixFusionRule"
    [ toJSON (Just (r1, r2))
    , toJSON (Just strictf)
    , toJSON f, toJSON g, toJSON h ]

{-| 
  (Unsafe) Fixed-point Fusion Rule
  Given f :: A -> B, g :: A -> A, h :: B -> B, and a proof that, for some x, 
  (f (g a) ==> x) and (h (f a) ==> x), then
  f (fix g) <==> fix h
  Note that the precondition that f is strict is required to hold.
-}
fixFusionRuleUnsafe :: String -> String -> String
                    -> Rewrite LCore -> Rewrite LCore -> BiRewrite LCore
fixFusionRuleUnsafe f g h r1 r2 = BiTransform $ method "fixFusionRule"
    [ toJSON (Just (r1, r2))
    , toJSON (Nothing :: Maybe (Rewrite LCore))
    , toJSON f, toJSON g, toJSON h ]

{-| 
  (Very Unsafe) Fixed-point Fusion Rule
  Given f :: A -> B, g :: A -> A, h :: B -> B, then f (fix g) <==> fix h
-}
fixFusionRuleVeryUnsafe :: String -> String -> String -> BiRewrite LCore
fixFusionRuleVeryUnsafe f g h = BiTransform $ method "fixFusionRule"
    [ toJSON (Nothing :: Maybe (Rewrite LCore, Rewrite LCore))
    , toJSON (Nothing :: Maybe (Rewrite LCore))
    , toJSON f, toJSON g, toJSON h ]

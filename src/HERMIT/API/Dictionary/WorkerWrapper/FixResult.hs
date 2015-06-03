{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.WorkerWrapper.FixResult where

import           HERMIT.API.Types

import           Data.Aeson
import           Data.String (fromString)

-- | "Worker/Wrapper Factorisation",
--   For any "f :: A -> A", and given "wrap :: B -> A" and "unwrap :: A -> B" as arguments,
--  and a proof of Assumption C (fix A (\ a -> wrap (unwrap (f a))) ==> fix A f), then
--   fix A f  ==>  wrap (fix B (\ b -> unwrap (f (wrap b))))
wwResultFactorisation :: String -> String -> RewriteH LCore -> BiRewriteH LCore
wwResultFactorisation abs rep assC
  = BiTransform $ method "wwResultFactorisation"
                        [ String $ fromString abs
                        , String $ fromString rep
                        , toJSON assC
                        ]

-- | Unsafe Worker/Wrapper Factorisation (Result Variant)
--  For any "f :: (X -> A) -> (X -> A)", and given "abs :: B -> A" and "rep :: A -> B" as arguments, then
--   fix (X->A) f  ==>  \ x1 -> abs (fix (X->B) (\ h x2 -> rep (f (\ x3 -> abs (h x3)) x2)) x1
--   Note: the pre-condition "fix (X -> A) (\ h x -> abs (rep (f h x))) == fix (X->A) f" is expected to hold.
wwResultFactorisationUnsafe :: String -> String -> BiRewriteH LCore
wwResultFactorisationUnsafe abs rep
  = BiTransform $ method "wwResultFactorisationUnsafe"
                         [ String $ fromString abs
                         , String $ fromString rep
                         ]


{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.WorkerWrapper.FixResult where

import           Data.Aeson
import           Data.String (fromString)
import           Data.Vector (fromList)

import           HERMIT.API.Types

import           Prelude hiding (abs)

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
-- | Worker/Wrapper Split (Result Variant)
--   For any "prog :: X -> A", and given "abs :: B -> A" and "rep :: A -> B" as arguments,
--   and a proof of Assumption C (fix (X->A) (\ h x -> abs (rep (f h x))) ==> fix (X->A) f), then
--   prog = expr  ==>  prog = let f = \ prog -> expr
--                             in let work = \ x1 -> rep (f (\ x2 -> abs (work x2)) x1)
--                                 in \ x0 -> abs (work x0)
wwResultSplit :: String -> String -> RewriteH LCore -> RewriteH LCore
wwResultSplit abs rep assC
  = Transform $ method "wwResultSplit"
                       [ String $ fromString abs
                       , String $ fromString rep
                       , toJSON assC
                       ]


-- | Unsafe Worker/Wrapper Split (Result Variant)
--   For any "prog :: X -> A", and given "abs :: B -> A" and "rep :: A -> B" as arguments, then
--   prog = expr  ==>  prog = let f = \ prog -> expr
--                             in let work = \ x1 -> rep (f (\ x2 -> abs (work x2)) x1)
--                                 in \ x0 -> abs (work x0)
--   Note: the pre-condition "fix (X->A) (\ h x -> abs (rep (f h x))) == fix (X->A) f" is expected to hold.
wwResultSplitUnsafe :: String -> String -> RewriteH LCore
wwResultSplitUnsafe abs rep
  = Transform $ method "wwResultSplitUnsafe"
                       [ String $ fromString abs
                       , String $ fromString rep
                       ]

-- | Worker/Wrapper Split - Static Argument Variant (Result Variant)
--   Perform the static argument transformation on the first n arguments, then perform the worker/wrapper split,
--   applying the given abs and rep functions to the specified (by index) static arguments before use.
wwResultSplitStaticArg :: Int -> [Int] -> String -> String -> RewriteH LCore -> RewriteH LCore
wwResultSplitStaticArg n is abs rep assC
  = Transform $ method "wwResultSplitStaticArg"
                       [ Number $ fromIntegral n
                       , Array . fromList $ map (Number . fromIntegral) is
                       , String $ fromString abs
                       , String $ fromString rep
                       , toJSON assC
                       ]

-- | Unsafe Worker/Wrapper Split - Static Argument Variant (Result Variant)
--   Perform the static argument transformation on the first n arguments, then perform the (unsafe) worker/wrapper split,
--   applying the given abs and rep functions to the specified (by index) static arguments before use.
wwResultSplitStaticArgUnsafe :: Int -> [Int] -> String -> String -> RewriteH LCore
wwResultSplitStaticArgUnsafe n is abs rep
  = Transform $ method "wwResultSplitStaticArgUnsafe"
                       [ Number $ fromIntegral n
                       , Array . fromList $ map (Number . fromIntegral) is
                       , String $ fromString abs
                       , String $ fromString rep
                       ]

-- | Worker/Wrapper Assumption A (Result Variant)
--   For a "abs :: B -> A" and a "rep :: A -> B",
--   and given a proof of "abs (rep a)  ==>  a", then
--   abs (rep a)  <==>  a
wwResultAssumptionA :: String -> String -> RewriteH LCore -> BiRewriteH LCore
wwResultAssumptionA abs rep assA
  = BiTransform $ method "wwResultAssumptionA"
                         [ String $ fromString abs
                         , String $ fromString rep
                         , toJSON assA
                         ]

-- |  Worker/Wrapper Assumption B (Result Variant)
--    For a "abs :: B -> A", an "rep :: A -> B", and an "f :: (X -> A) -> X -> A",
--    and given a proof of "abs (rep (f h x))  ==>  f h x", then
--    abs (rep (f h x))  <==>  f h x
wwResultAssumptionB :: String -> String -> String -> RewriteH LCore -> BiRewriteH LCore
wwResultAssumptionB abs rep f assA
  = BiTransform $ method "wwResultAssumptionB"
                         [ String $ fromString abs
                         , String $ fromString rep
                         , String $ fromString f
                         , toJSON assA
                         ]

-- |  Worker/Wrapper Assumption C (Result Variant)
--    For a "abs :: B -> A", an "rep :: A -> B", and an "f :: (X -> A) -> X -> A",
--    and given a proof of "fix (X->A) (\ h x -> abs (rep (f h x))) ==> fix (X->A) f", then
--    fix (X->A) (\ h x -> abs (rep (f h x)))  <==>  fix (X->A) f
wwResultAssumptionC :: String -> String -> String -> RewriteH LCore -> BiRewriteH LCore
wwResultAssumptionC abs rep f assA
  = BiTransform $ method "wwResultAssumptionC"
                         [ String $ fromString abs
                         , String $ fromString rep
                         , String $ fromString f
                         , toJSON assA
                         ]


-- |  Unsafe Worker/Wrapper Assumption A (Result Variant)
--    For a "abs :: B -> A" and a "rep :: A -> B", then
--    abs (rep a)  <==>  a
--    Note: only use this if it's true!
wwResultAssumptionAUnsafe :: String -> String -> BiRewriteH LCore
wwResultAssumptionAUnsafe abs rep
  = BiTransform $ method "wwResultAssumptionAUnsafe"
                         [ String $ fromString abs
                         , String $ fromString rep
                         ]


-- |  Unsafe Worker/Wrapper Assumption B (Result Variant)
--    For a "abs :: B -> A", an "rep :: A -> B", and an "f :: (X -> A) -> X -> A", then
--    abs (rep (f h x))  <==>  f h x
--    Note: only use this if it's true!
wwResultAssumptionBUnsafe :: String -> String -> String -> BiRewriteH LCore
wwResultAssumptionBUnsafe abs rep f
  = BiTransform $ method "wwResultAssumptionBUnsafe"
                         [ String $ fromString abs
                         , String $ fromString rep
                         , String $ fromString f
                         ]

-- |  Unsafe Worker/Wrapper Assumption C (Result Variant)
--    For a "abs :: B -> A", an "rep :: A -> B", and an "f :: (X -> A) -> X -> A", then
--    fix (X->A) (\ h x -> abs (rep (f h x)))  <==>  fix (X->A) f
--    Note: only use this if it's true!
wwResultAssumptionCUnsafe :: String -> String -> String -> BiRewriteH LCore
wwResultAssumptionCUnsafe abs rep f
  = BiTransform $ method "wwResultAssumptionCUnsafe"
                         [ String $ fromString abs
                         , String $ fromString rep
                         , String $ fromString f
                         ]

-- | Convert a proof of worker/wrapper Assumption A into a proof of worker/wrapper Assumption B.
wwResultAssAToAssB :: RewriteH LCore -> RewriteH LCore
wwResultAssAToAssB rewrite
  = Transform $ method "wwResultAssAToAssB"
                       [ toJSON rewrite
                       ]
-- | Convert a proof of worker/wrapper Assumption B into a proof of worker/wrapper Assumption C.
wwResultAssBToAssC :: RewriteH LCore -> RewriteH LCore
wwResultAssBToAssC rewrite
  = Transform $ method "wwResultAssBToAssC"
                       [ toJSON rewrite
                       ]

-- | Convert a proof of worker/wrapper Assumption A into a proof of worker/wrapper Assumption C.
wwResultAssAToAssC :: RewriteH LCore -> RewriteH LCore
wwResultAssAToAssC rewrite
  = Transform $ method "wwResultAssAToAssC"
                       [ toJSON rewrite
                       ]

-- | Given a proof of Assumption C (fix (X->A) (\ h x -> abs (rep (f h x))) ==> fix (X->A) f), then
--   execute this command on "work = \ x1 -> rep (f (\ x2 -> abs (work x2)) x1)" to enable the "wwResultFusion" rule thereafter.
--   Note that this is performed automatically as part of "wwResultSplit".
wwResultGenerateFusion :: RewriteH LCore -> Transform LCore ()
wwResultGenerateFusion rewrite
  = Transform $ method "wwResultGenerateFusion"
                       [ toJSON rewrite
                       ]

-- | Execute this command on "work = \ x1 -> rep (f (\ x2 -> abs (work x2)) x1)" to enable the "wwFusion" rule thereafter.
--   The precondition "fix (X->A) (\ h x -> abs (rep (f h x))) == fix (X->A) f" is expected to hold.
--   Note that this is performed automatically as part of "wwResultSplit".
wwResultGenerateFusionUnsafe :: Transform LCore ()
wwResultGenerateFusionUnsafe
  = Transform $ method "wwResultGenerateFusionUnsafe" []

-- | Worker/Wrapper Fusion (Result Variant)
--   rep (abs (work x))  <==>  work x
--   Note: you are required to have previously executed the command "wwGenerateFusion" on the definition
--         work = \ x1 -> rep (f (\ x2 -> abs (work x2)) x1)
wwResultFusion :: BiRewriteH LCore
wwResultFusion
  = BiTransform $ method "wwResultFusion" []


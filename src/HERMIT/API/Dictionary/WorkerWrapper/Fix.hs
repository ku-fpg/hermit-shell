{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.WorkerWrapper.Fix where

import HERMIT.API.Types
import Data.String (fromString)
import Data.Aeson
import Data.Vector (fromList)

-- | Worker/Wrapper Factorisation
--   For any "f :: A -> A", and given "wrap :: B -> A" and "unwrap :: A -> B" as arguments,
--   and a proof of Assumption C (fix A (\ a -> wrap (unwrap (f a))) ==> fix A f), then
--   fix A f  ==>  wrap (fix B (\ b -> unwrap (f (wrap b))))
wwFactorisation :: String -> String -> BiRewrite LCore -> BiRewrite LCore
wwFactorisation wrap unwrap assC
  = BiTransform $ method "wwFactorisation"
                         [ String $ fromString wrap
                         , String $ fromString unwrap
                         , toJSON assC
                         ]

-- | Unsafe Worker/Wrapper Factorisation
--   For any "f :: A -> A", and given "wrap :: B -> A" and "unwrap :: A -> B" as arguments, then
--   fix A f  <==>  wrap (fix B (\ b -> unwrap (f (wrap b))))
--   Note: the pre-condition "fix A (\ a -> wrap (unwrap (f a))) == fix A f" is expected to hold.
wwFactorisationUnsafe :: String -> String -> BiRewrite LCore
wwFactorisationUnsafe wrap unwrap
  = BiTransform $ method "wwFactorisationUnsafe"
                         [ String $ fromString wrap
                         , String $ fromString unwrap
                         ]

-- | Worker/Wrapper Split
--   For any "prog :: A", and given "wrap :: B -> A" and "unwrap :: A -> B" as arguments,
--   and a proof of Assumption C (fix A (\ a -> wrap (unwrap (f a))) ==> fix A f), then
--   prog = expr  ==>  prog = let f = \ prog -> expr
--                             in let work = unwrap (f (wrap work))
--                                 in wrap work
wwSplit :: String -> String -> Rewrite LCore -> Rewrite LCore
wwSplit wrap unwrap assC
  = Transform $ method "wwSplit"
                       [ String $ fromString wrap
                       , String $ fromString unwrap
                       , toJSON assC
                       ]

-- | Unsafe Worker/Wrapper Split
--   For any "prog :: A", and given "wrap :: B -> A" and "unwrap :: A -> B" as arguments, then
--   prog = expr  ==>  prog = let f = \ prog -> expr
--                             in let work = unwrap (f (wrap work))
--                                 in wrap work
--   Note: the pre-condition "fix A (wrap . unwrap . f) == fix A f" is expected to hold.
wwSplitUnsafe :: String -> String -> Rewrite LCore
wwSplitUnsafe wrap unwrap
  = Transform $ method "wwSplitUnsafe"
                       [ String $ fromString wrap
                       , String $ fromString unwrap
                       ]

-- | Worker/Wrapper Split - Static Argument Variant
--   Perform the static argument transformation on the first n arguments, then perform the worker/wrapper split,
--   applying the given wrap and unwrap functions to the specified (by index) static arguments before use.
wwSplitStaticArg :: Int -> [Int] -> String -> String -> Rewrite LCore -> Rewrite LCore
wwSplitStaticArg n is wrap unwrap assC
  = Transform $ method "wwSplitStaticArg"
                       [ Number $ fromIntegral n
                       , Array . fromList $ map (Number . fromIntegral) is
                       , String $ fromString wrap
                       , String $ fromString unwrap
                       , toJSON assC
                       ]

-- | Unsafe Worker/Wrapper Split - Static Argument Variant
--   Perform the static argument transformation on the first n arguments, then perform the (unsafe) worker/wrapper split,
--   applying the given wrap and unwrap functions to the specified (by index) static arguments before use.
wwSplitStaticArgUnsafe :: Int -> [Int] -> String -> String -> Rewrite LCore
wwSplitStaticArgUnsafe n is wrap unwrap
  = Transform $ method "wwSplitStaticArgUnsafe"
                       [ Number $ fromIntegral n
                       , Array . fromList $ map (Number . fromIntegral) is
                       , String $ fromString wrap
                       , String $ fromString unwrap
                       ]

-- | Worker/Wrapper Assumption A
--   For a "wrap :: B -> A" and an "unwrap :: A -> B",
--   and given a proof of "wrap (unwrap a) ==> a", then
--   wrap (unwrap a)  <==>  a
wwAssumptionA :: String -> String -> Rewrite LCore -> BiRewrite LCore
wwAssumptionA wrap unwrap assA
  = BiTransform $ method "wwAssumptionA"
                         [ String $ fromString wrap
                         , String $ fromString unwrap
                         , toJSON assA
                         ]
-- | Worker/Wrapper Assumption B
--   For a "wrap :: B -> A", an "unwrap :: A -> B", and an "f :: A -> A",
--   and given a proof of "wrap (unwrap (f a)) ==> f a", then
--   wrap (unwrap (f a))  <==>  f a
wwAssumptionB :: String -> String -> String -> Rewrite LCore -> BiRewrite LCore
wwAssumptionB wrap unwrap f assB
  = BiTransform $ method "wwAssumptionB"
                         [ String $ fromString wrap
                         , String $ fromString unwrap
                         , toJSON assB
                         ]

-- | Worker/Wrapper Assumption C
--   For a "wrap :: B -> A", an "unwrap :: A -> B", and an "f :: A -> A",
--   and given a proof of "fix A (\ a -> wrap (unwrap (f a))) ==> fix A f", then
--   fix A (\ a -> wrap (unwrap (f a)))  <==>  fix A f
wwAssumptionC :: String -> String -> String -> Rewrite LCore -> BiRewrite LCore
wwAssumptionC wrap unwrap f assC
  = BiTransform $ method "wwAssumptionC"
                         [ String $ fromString wrap
                         , String $ fromString unwrap
                         , toJSON assC
                         ]

-- | Unsafe Worker/Wrapper Assumption A
--   For a "wrap :: B -> A" and an "unwrap :: A -> B", then
--   wrap (unwrap a)  <==>  a
--   Note: only use this if it's true!
wwAssumptionAUnsafe :: String -> String -> BiRewrite LCore
wwAssumptionAUnsafe wrap unwrap
  = BiTransform $ method "wwAssumptionAUnsafe"
                         [ String $ fromString wrap
                         , String $ fromString unwrap
                         ]

-- | Unsafe Worker/Wrapper Assumption B
--   For a "wrap :: B -> A", an "unwrap :: A -> B", and an "f :: A -> A", then
--   wrap (unwrap (f a))  <==>  f a
--   Note: only use this if it's true!
wwAssumptionBUnsafe :: String -> String -> String -> BiRewrite LCore
wwAssumptionBUnsafe wrap unwrap f
  = BiTransform $ method "wwAssumptionBUnsafe"
                         [ String $ fromString wrap
                         , String $ fromString unwrap
                         ]

-- | Unsafe Worker/Wrapper Assumption C
--   For a "wrap :: B -> A", an "unwrap :: A -> B", and an "f :: A -> A", then
--   fix A (\ a -> wrap (unwrap (f a)))  <==>  fix A f
--   Note: only use this if it's true!
wwAssumptionCUnsafe :: String -> String -> String -> BiRewrite LCore
wwAssumptionCUnsafe wrap unwrap f
  = BiTransform $ method "wwAssumptionCUnsafe"
                         [ String $ fromString wrap
                         , String $ fromString unwrap
                         ]

-- | Convert a proof of worker/wrapper Assumption A into a proof of worker/wrapper Assumption B.
wwAssAToAssB :: Rewrite LCore -> Rewrite LCore
wwAssAToAssB rewrite
  = Transform $ method "wwAssAToAssB"
                       [ toJSON rewrite ]

-- | Convert a proof of worker/wrapper Assumption B into a proof of worker/wrapper Assumption C.
wwAssBToAssC :: Rewrite LCore -> Rewrite LCore
wwAssBToAssC rewrite
  = Transform $ method "wwAssBToAssC"
                       [ toJSON rewrite ]

-- | Convert a proof of worker/wrapper Assumption A into a proof of worker/wrapper Assumption C.
wwAssAToAssC :: Rewrite LCore -> Rewrite LCore
wwAssAToAssC rewrite
  = Transform $ method "wwAssAToAssC"
                       [ toJSON rewrite ]

-- | Given a proof of Assumption C (fix A (\ a -> wrap (unwrap (f a))) ==> fix A f), then
--   execute this command on "work = unwrap (f (wrap work))" to enable the "wwFusion" rule thereafter.
--   Note that this is performed automatically as part of "wwSplit".
wwGenerateFusion :: Rewrite LCore -> Transform LCore ()
wwGenerateFusion rewrite
  = Transform $ method "wwGenerateFusion"
                       [ toJSON rewrite ]

-- | Execute this command on "work = unwrap (f (wrap work))" to enable the "ww-fusion" rule thereafter.
--   The precondition "fix A (wrap . unwrap . f) == fix A f" is expected to hold.
--   Note that this is performed automatically as part of "ww-split".
wwGenerateFusionUnsafe :: Transform LCore ()
wwGenerateFusionUnsafe
  = Transform $ method "wwGenerateFusionUnsafe" []

-- | Worker/Wrapper Fusion
--   unwrap (wrap work)  <==>  work
--   Note: you are required to have previously executed the command "wwGenerateFusion" on the definition
--         work = unwrap (f (wrap work))
wwFusion :: BiRewrite LCore
wwFusion
  = BiTransform $ method "wwFusion" []


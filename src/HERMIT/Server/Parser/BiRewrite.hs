{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.Server.Parser.BiRewrite where

import HERMIT.Dictionary.FixPoint
import HERMIT.Dictionary.WorkerWrapper.Common
import HERMIT.Dictionary.WorkerWrapper.Fix
import HERMIT.ParserCore
import HERMIT.GHC
import HERMIT.External (CoreString)
import HERMIT.Kure

import HERMIT.Server.Parser.Rewrite ()
import HERMIT.Server.Parser.String ()
import HERMIT.Server.Parser.Utils

instance External (BiRewriteH LCore) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.FixPoint
      external "fixComputationRule" (promoteExprBiR fixComputationRuleBR :: BiRewriteH LCore)
        [ "Fixed-Point Computation Rule",
          "fix t f  <==>  f (fix t f)"
        ] .+ Context
    , external "fixRollingRule" (promoteExprBiR fixRollingRuleBR :: BiRewriteH LCore)
        [ "Rolling Rule",
          "fix tyA (\\ a -> f (g a))  <==>  f (fix tyB (\\ b -> g (f b))"
        ] .+ Context
    , external "fixFusionRule" ((\ f g h r1 r2 strictf -> promoteExprBiR
                                                                (fixFusionRule (Just (r1,r2)) (Just strictf) f g h))
                                                                :: CoreString -> CoreString -> CoreString
                                                                    -> RewriteH LCore -> RewriteH LCore
                                                                    -> RewriteH LCore -> BiRewriteH LCore)
        [ "Fixed-point Fusion Rule"
        , "Given f :: A -> B, g :: A -> A, h :: B -> B, and"
        , "proofs that, for some x, (f (g a) ==> x) and (h (f a) ==> x) and that f is strict, then"
        , "f (fix g) <==> fix h"
        ] .+ Context
    , external "fixFusionRuleUnsafe" ((\ f g h r1 r2 -> promoteExprBiR (fixFusionRule (Just (r1,r2)) Nothing f g h))
                                                            :: CoreString -> CoreString -> CoreString
                                                                -> RewriteH LCore -> RewriteH LCore -> BiRewriteH LCore)
        [ "(Unsafe) Fixed-point Fusion Rule"
        , "Given f :: A -> B, g :: A -> A, h :: B -> B, and"
        , "a proof that, for some x, (f (g a) ==> x) and (h (f a) ==> x), then"
        , "f (fix g) <==> fix h"
        , "Note that the precondition that f is strict is required to hold."
        ] .+ Context .+ PreCondition
    , external "fixFusionRuleUnsafe" ((\ f g h -> promoteExprBiR (fixFusionRule Nothing Nothing f g h))
                                                        :: CoreString -> CoreString -> CoreString -> BiRewriteH LCore)
        [ "(Very Unsafe) Fixed-point Fusion Rule"
        , "Given f :: A -> B, g :: A -> A, h :: B -> B, then"
        , "f (fix g) <==> fix h"
        , "Note that the preconditions that f (g a) == h (f a) and that f is strict are required to hold."
        ] .+ Context .+ PreCondition

    , external "wwResultFactorisation" ((\ abs rep assC -> promoteExprBiR $ wwFac (mkWWAssC assC) abs rep)
                                          :: CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
                [ "Worker/Wrapper Factorisation (Result Variant)",
                  "For any \"f :: (X -> A) -> (X -> A)\", and given \"abs :: B -> A\" and \"rep :: A -> B\" as arguments,",
                  "and a proof of Assumption C (fix (X -> A) (\\ h x -> abs (rep (f h x))) ==> fix (X->A) f), then",
                  "fix (X->A) f  ==>  \\ x1 -> abs (fix (X->B) (\\ h x2 -> rep (f (\\ x3 -> abs (h x3)) x2)) x1"
                ] .+ Introduce .+ Context
    , external "wwResultFactorisationUnsafe" ((\ wrap unwrap -> promoteExprBiR $ wwFac Nothing wrap unwrap)
                                               :: CoreString -> CoreString -> BiRewriteH LCore)
                [ "Unsafe Worker/Wrapper Factorisation",
                  "For any \"f :: A -> A\", and given \"wrap :: B -> A\" and \"unwrap :: A -> B\" as arguments, then",
                  "fix A f  <==>  wrap (fix B (\\ b -> unwrap (f (wrap b))))",
                  "Note: the pre-condition \"fix A (\\ a -> wrap (unwrap (f a))) == fix A f\" is expected to hold."
                ] .+ Introduce .+ Context .+ PreCondition
    ]
    where
      mkWWAssC :: RewriteH LCore -> Maybe WWAssumption
      mkWWAssC r = Just (WWAssumption C (extractR r))

-- | For any @f :: A -> A@, and given @wrap :: B -> A@ and @unwrap :: A -> B@ as arguments, then
--   @fix A f@  \<==\>  @wrap (fix B (\\ b -> unwrap (f (wrap b))))@
wwFac :: Maybe WWAssumption -> CoreString -> CoreString -> BiRewriteH CoreExpr
wwFac mAss = parse2beforeBiR (wwFacBR mAss)



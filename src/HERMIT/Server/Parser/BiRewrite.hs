{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.Server.Parser.BiRewrite where

import HERMIT.Dictionary.FixPoint
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

      -- HERMIT.API.Dictionary.KURE
--     , external ">>>"        ((>>>) :: BiRewriteH LCore -> BiRewriteH LCore -> BiRewriteH LCore)
--         [ "Compose bidirectional rewrites, requiring both to succeed." ]
--     , external "invert"     (invertBiT :: BiRewriteH LCore -> BiRewriteH LCore)
--         [ "Reverse a bidirectional rewrite." ]
    ]

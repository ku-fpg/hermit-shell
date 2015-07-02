{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.BiRewrite () where

import                Control.Arrow

import                HERMIT.Dictionary
import                HERMIT.External (CoreString)
import                HERMIT.GHC
import                HERMIT.Kure
import                HERMIT.Lemma
import                HERMIT.ParserCore

import                Prelude hiding (abs)

import                HERMIT.Server.Parser.Name ()
import {-# SOURCE #-} HERMIT.Server.Parser.Rewrite ()
import                HERMIT.Server.Parser.String ()
import                HERMIT.Server.Parser.Utils

-------------------------------------------------------------------------------

instance External (BiRewriteH LCore) where
  parseExternals =
    [
-- HERMIT.API.Dictionary.FixPoint
      external "fixComputationRule"
        (promoteExprBiR fixComputationRuleBR :: BiRewriteH LCore)
        [ "Fixed-Point Computation Rule",
          "fix t f  <==>  f (fix t f)"
        ] .+ Context

    , external "fixRollingRule"
        (promoteExprBiR fixRollingRuleBR :: BiRewriteH LCore)
        [ "Rolling Rule",
          "fix tyA (\\ a -> f (g a))  <==>  f (fix tyB (\\ b -> g (f b))"
        ] .+ Context

    , external "fixFusionRule"
        ((\ f g h r1 r2 strictf ->
            promoteExprBiR (fixFusionRule (Just (r1,r2)) (Just strictf) f g h))
        :: CoreString -> CoreString -> CoreString -> RewriteH LCore
        -> RewriteH LCore -> RewriteH LCore -> BiRewriteH LCore)
        [ "FixedPoint Fusion Rule"
        , "Given f :: A -> B, g :: A -> A, h :: B -> B, and"
        , "proofs that, for some x, (f (g a) ==> x) and (h (f a) ==> x) " ++
          "and that f is strict, then"
        , "f (fix g) <==> fix h"
        ] .+ Context

-- HERMIT.API.Dictionary.KURE
    , external ">>>"
        ((>>>) :: BiRewriteH LCore -> BiRewriteH LCore -> BiRewriteH LCore)
        [ "Compose bidirectional rewrites, requiring both to succeed." ]

    , external "invert"
        (invertBiT :: BiRewriteH LCore -> BiRewriteH LCore)
        [ "Reverse a bidirectional rewrite." ]

-- ??
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
                  "Note: the preCondition \"fix A (\\ a -> wrap (unwrap (f a))) == fix A f\" is expected to hold."
                ] .+ Introduce .+ Context .+ PreCondition
    , external "wwResultAssumptionA" ((\ abs rep assA -> promoteExprBiR $ wwA (Just $ extractR assA) abs rep)
                                       :: CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
                [ "Worker/Wrapper Assumption A (Result Variant)",
                  "For a \"abs :: B -> A\" and a \"rep :: A -> B\",",
                  "and given a proof of \"abs (rep a)  ==>  a\", then",
                  "abs (rep a)  <==>  a"
                ] .+ Introduce .+ Context
    , external "wwResultAssumptionB" ((\ abs rep f assB -> promoteExprBiR $ wwB (Just $ extractR assB) abs rep f)
                                       :: CoreString -> CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
                [ "Worker/Wrapper Assumption B (Result Variant)",
                  "For a \"abs :: B -> A\", an \"rep :: A -> B\", and an \"f :: (X -> A) -> X -> A\",",
                  "and given a proof of \"abs (rep (f h x))  ==>  f h x\", then",
                  "abs (rep (f h x))  <==>  f h x"
                ] .+ Introduce .+ Context
    , external "wwResultAssumptionC" ((\ abs rep f assC -> promoteExprBiR $ wwC (Just $ extractR assC) abs rep f)
                                       :: CoreString -> CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
                [ "Worker/Wrapper Assumption C (Result Variant)",
                  "For a \"abs :: B -> A\", an \"rep :: A -> B\", and an \"f :: (X -> A) -> X -> A\",",
                  "and given a proof of \"fix (X->A) (\\ h x -> abs (rep (f h x))) ==> fix (X->A) f\", then",
                  "fix (X->A) (\\ h x -> abs (rep (f h x)))  <==>  fix (X->A) f"
                ] .+ Introduce .+ Context
    , external "wwResultAssumptionAUnsafe" ((\ abs rep -> promoteExprBiR $ wwA Nothing abs rep)
                                              :: CoreString -> CoreString -> BiRewriteH LCore)
                [ "Unsafe Worker/Wrapper Assumption A (Result Variant)",
                  "For a \"abs :: B -> A\" and a \"rep :: A -> B\", then",
                  "abs (rep a)  <==>  a",
                  "Note: only use this if it's true!"
                ] .+ Introduce .+ Context .+ PreCondition
    , external "wwResultAssumptionBUnsafe" ((\ abs rep f -> promoteExprBiR $ wwB Nothing abs rep f)
                                              :: CoreString -> CoreString -> CoreString -> BiRewriteH LCore)
                [ "Unsafe Worker/Wrapper Assumption B (Result Variant)",
                  "For a \"abs :: B -> A\", an \"rep :: A -> B\", and an \"f :: (X -> A) -> X -> A\", then",
                  "abs (rep (f h x))  <==>  f h x",
                  "Note: only use this if it's true!"
                ] .+ Introduce .+ Context .+ PreCondition
    , external "wwResultFusion" (promoteExprBiR wwFusion :: BiRewriteH LCore)
                [ "Worker/Wrapper Fusion (Result Variant)",
                  "rep (abs (work x))  <==>  work x",
                  "Note: you are required to have previously executed the command \"ww-generateFusion\" on the definition",
                  "      work = \\ x1 -> rep (f (\\ x2 -> abs (work x2)) x1)"
                ] .+ Introduce .+ Context .+ PreCondition .+ TODO

    , external "wwFactorisation" ((\ wrap unwrap assC -> promoteExprBiR $ wwFac (mkWWAssC assC) wrap unwrap)
                                          :: CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
                [ "Worker/Wrapper Factorisation",
                  "For any \"f :: A -> A\", and given \"wrap :: B -> A\" and \"unwrap :: A -> B\" as arguments,",
                  "and a proof of Assumption C (fix A (\\ a -> wrap (unwrap (f a))) ==> fix A f), then",
                  "fix A f  ==>  wrap (fix B (\\ b -> unwrap (f (wrap b))))"
                ] .+ Introduce .+ Context
    , external "wwFactorisationUnsafe" ((\ wrap unwrap -> promoteExprBiR $ wwFac Nothing wrap unwrap)
                                               :: CoreString -> CoreString -> BiRewriteH LCore)
                [ "Unsafe Worker/Wrapper Factorisation",
                  "For any \"f :: A -> A\", and given \"wrap :: B -> A\" and \"unwrap :: A -> B\" as arguments, then",
                  "fix A f  <==>  wrap (fix B (\\ b -> unwrap (f (wrap b))))",
                  "Note: the preCondition \"fix A (\\ a -> wrap (unwrap (f a))) == fix A f\" is expected to hold."
                ] .+ Introduce .+ Context .+ PreCondition
    , external "wwAssumptionA" ((\ wrap unwrap assA -> promoteExprBiR $ wwA (Just $ extractR assA) wrap unwrap)
                                       :: CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
                [ "Worker/Wrapper Assumption A",
                  "For a \"wrap :: B -> A\" and an \"unwrap :: A -> B\",",
                  "and given a proof of \"wrap (unwrap a) ==> a\", then",
                  "wrap (unwrap a)  <==>  a"
                ] .+ Introduce .+ Context
    , external "wwAssumptionB" ((\ wrap unwrap f assB -> promoteExprBiR $ wwB (Just $ extractR assB) wrap unwrap f)
                                       :: CoreString -> CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
                [ "Worker/Wrapper Assumption B",
                  "For a \"wrap :: B -> A\", an \"unwrap :: A -> B\", and an \"f :: A -> A\",",
                  "and given a proof of \"wrap (unwrap (f a)) ==> f a\", then",
                  "wrap (unwrap (f a))  <==>  f a"
                ] .+ Introduce .+ Context
    , external "wwAssumptionC" ((\ wrap unwrap f assC -> promoteExprBiR $ wwC (Just $ extractR assC) wrap unwrap f)
                                       :: CoreString -> CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
                [ "Worker/Wrapper Assumption C",
                  "For a \"wrap :: B -> A\", an \"unwrap :: A -> B\", and an \"f :: A -> A\",",
                  "and given a proof of \"fix A (\\ a -> wrap (unwrap (f a))) ==> fix A f\", then",
                  "fix A (\\ a -> wrap (unwrap (f a)))  <==>  fix A f"
                ] .+ Introduce .+ Context
    , external "wwAssumptionAUnsafe" ((\ wrap unwrap -> promoteExprBiR $ wwA Nothing wrap unwrap)
                                              :: CoreString -> CoreString -> BiRewriteH LCore)
                [ "Unsafe Worker/Wrapper Assumption A",
                  "For a \"wrap :: B -> A\" and an \"unwrap :: A -> B\", then",
                  "wrap (unwrap a)  <==>  a",
                  "Note: only use this if it's true!"
                ] .+ Introduce .+ Context .+ PreCondition
    , external "wwAssumptionBUnsafe" ((\ wrap unwrap f -> promoteExprBiR $ wwB Nothing wrap unwrap f)
                                              :: CoreString -> CoreString -> CoreString -> BiRewriteH LCore)
                [ "Unsafe Worker/Wrapper Assumption B",
                  "For a \"wrap :: B -> A\", an \"unwrap :: A -> B\", and an \"f :: A -> A\", then",
                  "wrap (unwrap (f a))  <==>  f a",
                  "Note: only use this if it's true!"
                ] .+ Introduce .+ Context .+ PreCondition
    , external "wwAssumptionCUnsafe" ((\ wrap unwrap f -> promoteExprBiR $ wwC Nothing wrap unwrap f)
                                              :: CoreString -> CoreString -> CoreString -> BiRewriteH LCore)
                [ "Unsafe Worker/Wrapper Assumption C",
                  "For a \"wrap :: B -> A\", an \"unwrap :: A -> B\", and an \"f :: A -> A\", then",
                  "fix A (\\ a -> wrap (unwrap (f a)))  <==>  fix A f",
                  "Note: only use this if it's true!"
                ] .+ Introduce .+ Context .+ PreCondition
    , external "wwFusion" (promoteExprBiR wwFusion :: BiRewriteH LCore)
                [ "Worker/Wrapper Fusion",
                  "unwrap (wrap work)  <==>  work",
                  "Note: you are required to have previously executed the command \"ww-generateFusion\" on the definition",
                  "      work = unwrap (f (wrap work))"
                ] .+ Introduce .+ Context .+ PreCondition .+ TODO

    , external "retraction" ((\ f g r -> promoteExprBiR $ retraction (Just r) f g) :: CoreString -> CoreString -> RewriteH LCore -> BiRewriteH LCore)
        [ "Given f :: X -> Y and g :: Y -> X, and a proof that f (g y) ==> y, then"
        , "f (g y) <==> y."
        ] .+ Shallow
    , external "retractionUnsafe" ((\ f g -> promoteExprBiR $ retraction Nothing f g) :: CoreString -> CoreString -> BiRewriteH LCore)
        [ "Given f :: X -> Y and g :: Y -> X, then"
        , "f (g y) <==> y."
        , "Note that the precondition (f (g y) == y) is expected to hold."
        ] .+ Shallow .+ PreCondition
    , external "lemmaBirewrite" (promoteExprBiR . lemmaBiR Obligation :: LemmaName -> BiRewriteH LCore)
        [ "Generate a bi-directional rewrite from a lemma." ]
        , external "lemmaConsequentBirewrite" (promoteExprBiR . lemmaConsequentBiR Obligation :: LemmaName -> BiRewriteH LCore)
        [ "Generate a bi-directional rewrite from the consequent of an implication lemma."
        , "The antecedent is instantiated and introduced as an unproven obligation." ]
    ]
    where
      mkWWAssC :: RewriteH LCore -> Maybe WWAssumption
      mkWWAssC r = Just (WWAssumption C (extractR r))

-- | For any @f :: A -> A@, and given @wrap :: B -> A@ and @unwrap :: A -> B@ as arguments, then
--   @fix A f@  \<==\>  @wrap (fix B (\\ b -> unwrap (f (wrap b))))@
wwFac :: Maybe WWAssumption -> CoreString -> CoreString -> BiRewriteH CoreExpr
wwFac mAss = parse2beforeBiR (wwFacBR mAss)

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Transform where

import           Control.Arrow
import           Control.Monad

import           Data.Proxy

import           HERMIT.Context
import           HERMIT.Dictionary
import           HERMIT.External (CoreString)
import           HERMIT.GHC
import           HERMIT.Kure
import           HERMIT.Lemma
import           HERMIT.Name
import           HERMIT.ParserCore
import           HERMIT.PrettyPrinter.Common

import           HERMIT.Server.Parser.Name ()
import           HERMIT.Server.Parser.String ()
import           HERMIT.Server.Parser.Utils

import           Prelude hiding (abs)
import           Data.String (fromString)

-------------------------------------------------------------------------------

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
        [ "FixedPoint Fusion Rule"
        , "Given f :: A -> B, g :: A -> A, h :: B -> B, and"
        , "proofs that, for some x, (f (g a) ==> x) and (h (f a) ==> x) and that f is strict, then"
        , "f (fix g) <==> fix h"
        ] .+ Context
    , external "fixFusionRuleUnsafe" ((\ f g h r1 r2 -> promoteExprBiR (fixFusionRule (Just (r1,r2)) Nothing f g h))
                                                            :: CoreString -> CoreString -> CoreString
                                                                -> RewriteH LCore -> RewriteH LCore -> BiRewriteH LCore)
        [ "(Unsafe) FixedPoint Fusion Rule"
        , "Given f :: A -> B, g :: A -> A, h :: B -> B, and"
        , "a proof that, for some x, (f (g a) ==> x) and (h (f a) ==> x), then"
        , "f (fix g) <==> fix h"
        , "Note that the precondition that f is strict is required to hold."
        ] .+ Context .+ PreCondition
    , external "fixFusionRuleUnsafe" ((\ f g h -> promoteExprBiR (fixFusionRule Nothing Nothing f g h))
                                                        :: CoreString -> CoreString -> CoreString -> BiRewriteH LCore)
        [ "(Very Unsafe) FixedPoint Fusion Rule"
        , "Given f :: A -> B, g :: A -> A, h :: B -> B, then"
        , "f (fix g) <==> fix h"
        , "Note that the preconditions that f (g a) == h (f a) and that f is strict are required to hold."
        ] .+ Context .+ PreCondition

      -- HERMIT.API.Dictionary.KURE
    , external ">>>"        ((>>>) :: BiRewriteH LCore -> BiRewriteH LCore -> BiRewriteH LCore)
        [ "Compose bidirectional rewrites, requiring both to succeed." ]
    , external "invert"     (invertBiT :: BiRewriteH LCore -> BiRewriteH LCore)
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

-------------------------------------------------------------------------------

instance External (RewriteH LCore) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.AlphaConversion
      external "alpha" (promoteCoreR alphaR :: RewriteH LCore)
        [ "Renames the bound variables at the current node."]
    , external "alphaCase" (promoteExprR alphaCaseR :: RewriteH LCore)
        [ "Renames all binders in a Case alternative."]
    , external "alphaProg" (promoteProgR alphaProgR :: RewriteH LCore)
        [ "Rename all topLevel identifiers in the program."]
    ,  external "unshadow" (promoteCoreR unshadowR :: RewriteH LCore)
        [ "Rename local variables with manifestly unique names (x, x0, x1, ...)."]

      -- HERMIT.API.Dictionary.Composite
    , external "unfoldBasicCombinator" (promoteExprR unfoldBasicCombinatorR :: RewriteH LCore)
        [ "Unfold the current expression if it is one of the basic combinators:"
        , "($), (.), id, flip, const, fst, snd, curry, and uncurry." ]
    , external "simplify" (simplifyR :: RewriteH LCore)
        [ "innermost (unfoldBasicCombinator <+ betaReducePlus <+ safeLetSubst <+ caseReduce <+ letElim)" ]
    , external "bash" (bashR :: RewriteH LCore)
        bashHelp .+ Eval .+ Deep .+ Loop
    , external "smash" (smashR :: RewriteH LCore)
        smashHelp .+ Eval .+ Deep .+ Loop .+ Experiment
    , external "bashExtendedWith" (bashExtendedWithR :: [RewriteH LCore] -> RewriteH LCore)
        [ "Run \"bash\" extended with additional rewrites.",
          "Note: be sure that the new rewrite either fails or makes progress, else this may loop."
        ] .+ Eval .+ Deep .+ Loop
    , external "smashExtendedWith" (smashExtendedWithR :: [RewriteH LCore] -> RewriteH LCore)
        [ "Run \"smash\" extended with additional rewrites.",
          "Note: be sure that the new rewrite either fails or makes progress, else this may loop."
        ] .+ Eval .+ Deep .+ Loop
    , external "bashDebug" (bashDebugR :: RewriteH LCore)
        [ "verbose bash - most useful with setAutoCorelint True" ] .+ Eval .+ Deep .+ Loop

      -- HERMIT.API.Dictionary.FixPoint
    , external "fixIntro" (promoteCoreR fixIntroR :: RewriteH LCore)
        [ "rewrite a function binding into a nonRecursive binding using fix" ] .+ Introduce .+ Context

      -- HERMIT.API.Dictionary.Fold
    , external "fold" (promoteExprR . foldR :: HermitName -> RewriteH LCore)
        [ "fold a definition"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        , ""
        , "5 + 5 + 6"
        , "anyBu (fold 'double)"
        , "double 5 + 6"
        , ""
        , "Note: due to associativity, if you wanted to fold 5 + 6 + 6, "
        , "you first need to apply an associativity rewrite." ]  .+ Context .+ Deep

      -- HERMIT.API.Dictionary.Function
    , external "staticArg" (promoteDefR staticArgR :: RewriteH LCore)
        [ "perform the static argument transformation on a recursive function." ]
    , external "staticArgTypes" (promoteDefR staticArgTypesR :: RewriteH LCore)
        [ "perform the static argument transformation on a recursive function, only transforming type arguments." ]
--     , external "staticArgPos" (promoteDefR . staticArgPosR :: [Int] -> RewriteH LCore)
--         [ "perform the static argument transformation on a recursive function, only transforming the arguments specified (by index)." ]

      -- HERMIT.API.Dictionary.GHC
    , external "deshadowProg" (promoteProgR deShadowProgR :: RewriteH LCore)
        [ "Deshadow a program." ] .+ Deep
    , external "dezombify" (promoteExprR dezombifyR :: RewriteH LCore)
        [ "Zap the occurrence information in the current identifer if it is a zombie."] .+ Shallow
    , external "occurrenceAnalysis" (occurrenceAnalysisR :: RewriteH LCore)
        [ "Perform dependency analysis on all subExpressions; simplifying and updating identifer info."] .+ Deep

      -- HERMIT.API.Dictionary.Induction
    , external "induction" (promoteClauseR . caseSplitOnR True . cmpHN2Var :: HermitName -> RewriteH LCore)
        [ "Induct on specified value quantifier." ]
    , external "proveByCases" (promoteClauseR . caseSplitOnR False . cmpHN2Var :: HermitName -> RewriteH LCore)
        [ "Case split on specified value quantifier." ]

      -- HERMIT.API.Dictionary.Inline
--     , external "inline" (promoteExprR inlineR :: RewriteH LCore)
--         [ "(Var v) ==> <defn of v>" ].+ Eval .+ Deep
--     , external "inline" (promoteExprR . inlineMatchingPredR . mkOccPred :: OccurrenceName -> RewriteH LCore)
--         [ "Given a specific v, (Var v) ==> <defn of v>" ] .+ Eval .+ Deep
--     , external "inline" (promoteExprR . inlineNamesR :: [String] -> RewriteH LCore)
--         [ "If the current variable matches any of the given names, then inline it." ] .+ Eval .+ Deep
    , external "inlineCaseScrutinee" (promoteExprR inlineCaseScrutineeR :: RewriteH LCore)
        [ "if v is a case binder, replace (Var v) with the bound case scrutinee." ] .+ Eval .+ Deep
    , external "inlineCaseAlternative" (promoteExprR inlineCaseAlternativeR :: RewriteH LCore)
        [ "if v is a case binder, replace (Var v) with the bound caseAlternative pattern." ] .+ Eval .+ Deep

      -- HERMIT.API.Dictionary.KURE
    , external "id_"         (idR :: RewriteH LCore)
        [ "Perform an identity rewrite."] .+ Shallow
    , external "fail_"       (fail :: String -> RewriteH LCore)
        [ "A failing rewrite."]
    , external "<+"         ((<+) :: RewriteH LCore -> RewriteH LCore -> RewriteH LCore)
        [ "Perform the first rewrite, and then, if it fails, perform the second rewrite." ]
    , external ">>>"        ((>>>) :: RewriteH LCore -> RewriteH LCore -> RewriteH LCore)
        [ "Compose rewrites, requiring both to succeed." ]
    , external ">+>"        ((>+>) :: RewriteH LCore -> RewriteH LCore -> RewriteH LCore)
        [ "Compose rewrites, allowing one to fail." ]
    , external "try"        (tryR :: RewriteH LCore -> RewriteH LCore)
        [ "Try a rewrite, and perform the identity if the rewrite fails." ]
    , external "repeat"     (repeatR :: RewriteH LCore -> RewriteH LCore)
        [ "Repeat a rewrite until it would fail." ] .+ Loop
    , external "replicate"  ((\ n -> andR . replicate n)  :: Int -> RewriteH LCore -> RewriteH LCore)
        [ "Repeat a rewrite n times." ]
    , external "all"        (allR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to all children of the node, requiring success at every child." ] .+ Shallow
    , external "any"        (anyR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to all children of the node, requiring success for at least one child." ] .+ Shallow
    , external "one"        (oneR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to the first child of the node for which it can succeed." ] .+ Shallow
    , external "allBU"     (allbuR :: RewriteH LCore -> RewriteH LCore)
        [ "Promote a rewrite to operate over an entire tree in bottomUp order, requiring success at every node." ] .+ Deep
    , external "allTD"     (alltdR :: RewriteH LCore -> RewriteH LCore)
        [ "Promote a rewrite to operate over an entire tree in top-down order, requiring success at every node." ] .+ Deep
    , external "allDU"     (allduR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite twice, in a top-down and bottomUp way, using one single tree traversal,",
          "succeeding if they all succeed."] .+ Deep
    , external "anyBU"     (anybuR :: RewriteH LCore -> RewriteH LCore)
        [ "Promote a rewrite to operate over an entire tree in bottomUp order, requiring success for at least one node." ] .+ Deep
    , external "anyTD"     (anytdR :: RewriteH LCore -> RewriteH LCore)
        [ "Promote a rewrite to operate over an entire tree in top-down order, requiring success for at least one node." ] .+ Deep
    , external "anyDU"     (anyduR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite twice, in a top-down and bottomUp way, using one single tree traversal,",
          "succeeding if any succeed."] .+ Deep
    , external "oneTD"     (onetdR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to the first node (in a top-down order) for which it can succeed." ] .+ Deep
    , external "oneBU"     (onebuR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to the first node (in a bottomUp order) for which it can succeed." ] .+ Deep
    , external "pruneTD"   (prunetdR :: RewriteH LCore -> RewriteH LCore)
        [ "Attempt to apply a rewrite in a top-down manner, prunning at successful rewrites." ] .+ Deep
    , external "innermost"  (innermostR :: RewriteH LCore -> RewriteH LCore)
        [ "A fixedPoint traveral, starting with the innermost term." ] .+ Deep .+ Loop
--     , external "focus"      (hfocusR :: TransformH LCore LocalPathH -> RewriteH LCore -> RewriteH LCore)
--         [ "Apply a rewrite to a focal point."] .+ Navigation .+ Deep
--     , external "focus"      ((\p -> hfocusR (return p)) :: LocalPathH -> RewriteH LCore -> RewriteH LCore)
--         [ "Apply a rewrite to a focal point."] .+ Navigation .+ Deep
    , external "when"       ((>>) :: TransformH LCore () -> RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite only if the check succeeds." ] .+ Predicate
    , external "forward"    (forwardT :: BiRewriteH LCore -> RewriteH LCore)
        [ "Apply a bidirectional rewrite forewards." ]
    , external "backward"   (backwardT :: BiRewriteH LCore -> RewriteH LCore)
        [ "Apply a bidirectional rewrite backwards." ]
    , external "anyCall"   (const anyCallR_LCore :: Proxy LCore -> RewriteH LCore -> RewriteH LCore)
        [ "anyCall (.. unfold command ..) applies an unfold command to all applications."
        , "Preference is given to applications with more arguments." ] .+ Deep
--     , external "extract"    (extractR :: RewriteH LCoreTC -> RewriteH LCore)
--         [ "Extract a RewriteCore from a RewriteCoreTC" ]
--    , external "atPath"     (flip hfocusT idR :: TransformH LCore LocalPathH -> TransformH LCore LCore)
--        [ "return the expression found at the given path" ]
--    , external "atPath"     (extractT . flip hfocusT projectT :: TransformH LCoreTC LocalPathH -> TransformH LCore LCore)
--        [ "return the expression found at the given path" ]

      -- HERMIT.API.Dictionary.Local
    , external "betaReduce" (promoteExprR betaReduceR :: RewriteH LCore)
        [ "((\\ v -> E1) E2) ==> let v = E2 in E1"
        , "This form of beta-reduction is safe if E2 is an arbitrary expression"
        , "(won't duplicate work)." ]                                 .+ Eval .+ Shallow
    , external "betaExpand" (promoteExprR betaExpandR :: RewriteH LCore)
        [ "(let v = e1 in e2) ==> (\\ v -> e2) e1" ]                            .+ Shallow
    , external "etaReduce" (promoteExprR etaReduceR :: RewriteH LCore)
        [ "(\\ v -> e1 v) ==> e1" ]                                             .+ Eval .+ Shallow
    , external "etaExpand" (promoteExprR . etaExpandR :: String -> RewriteH LCore)
        [ "\"eta-expand 'v\" performs e1 ==> (\\ v -> e1 v)" ]                  .+ Shallow .+ Introduce
    , external "flattenModule" (promoteModGutsR flattenModuleR :: RewriteH LCore)
        [ "Flatten all the top-level binding groups in the module to a single recursive binding group."
        , "This can be useful if you intend to appply GHC RULES." ]
    , external "flattenProgram" (promoteProgR flattenProgramR :: RewriteH LCore)
        [ "Flatten all the top-level binding groups in a program (list of binding groups) to a single"
        , "recursive binding group.  This can be useful if you intend to apply GHC RULES." ]
    , external "abstract" (promoteExprR . abstractR . mkOccPred :: OccurrenceName -> RewriteH LCore)
        [ "Abstract over a variable using a lambda."
        , "e  ==>  (\\ x -> e) x" ]                                             .+ Shallow .+ Introduce .+ Context
    , external "push" ((\ nm strictf -> push (Just strictf) (cmpString2Var nm)) :: String -> RewriteH LCore -> RewriteH LCore)
        [ "Push a function 'f into a case-expression or let-expression argument,"
        , "given a proof that f (fully saturated with type arguments) is strict." ] .+ Shallow .+ Commute
    , external "pushUnsafe" (push Nothing . cmpString2Var :: String -> RewriteH LCore)
        [ "Push a function 'f into a case-expression or let-expression argument."
        , "Requires 'f to be strict." ] .+ Shallow .+ Commute .+ PreCondition .+ Unsafe

      -- HERMIT.API.Dictionary.Local.Bind
    , external "nonrecToRec" (promoteBindR nonrecToRecR :: RewriteH LCore)
        [ "Convert a nonRecursive binding into a recursive binding group with a single definition."
        , "NonRec v e ==> Rec [Def v e]" ]                           .+ Shallow
    , external "recToNonrec" (promoteBindR recToNonrecR :: RewriteH LCore)
        [ "Convert a singleton recursive binding into a nonRecursive binding group."
        , "Rec [Def v e] ==> NonRec v e,  (v not free in e)" ]

      -- HERMIT.API.Dictionary.Local.Case
    , external "caseFloatApp" (promoteExprR caseFloatAppR :: RewriteH LCore)
        [ "(case ec of alt -> e) v ==> case ec of alt -> e v" ] .+ Commute .+ Shallow
    , external "caseFloatArg" (promoteExprR . caseFloatArg Nothing . Just :: RewriteH LCore -> RewriteH LCore)
        [ "Given a proof that f is strict, then"
        , "f (case s of alt -> e) ==> case s of alt -> f e" ]   .+ Commute .+ Shallow
    , external "caseFloatArg" ((\ f strict -> promoteExprR (caseFloatArg (Just f) (Just strict))) :: CoreString -> RewriteH LCore -> RewriteH LCore)
        [ "For a specified f, given a proof that f is strict, then"
        , "f (case s of alt -> e) ==> case s of alt -> f e" ]   .+ Commute .+ Shallow
    , external "caseFloatArgUnsafe" ((\ f -> promoteExprR (caseFloatArg (Just f) Nothing)) :: CoreString -> RewriteH LCore)
        [ "For a specified f,"
        , "f (case s of alt -> e) ==> case s of alt -> f e" ]   .+ Commute .+ Shallow .+ PreCondition .+ Strictness
    , external "caseFloatArgUnsafe" (promoteExprR . caseFloatArgLemmaR UnsafeUsed :: LemmaName -> RewriteH LCore)
        [ "f (case s of alt -> e) ==> case s of alt -> f e" ]   .+ Commute .+ Shallow .+ PreCondition .+ Strictness .+ Unsafe
    , external "caseFloatArgLemma" (promoteExprR . caseFloatArgLemmaR Obligation :: LemmaName -> RewriteH LCore)
        [ "f (case s of alt -> e) ==> case s of alt -> f e"
        , "Generates a lemma with given name for strictness side condition on f." ] .+ Commute .+ Shallow .+ PreCondition .+ Strictness
    , external "caseFloatCase" (promoteExprR caseFloatCaseR :: RewriteH LCore)
        [ "case (case ec of alt1 -> e1) of alta -> ea ==> case ec of alt1 -> case e1 of alta -> ea" ] .+ Commute .+ Eval
    , external "caseFloatCase" (promoteExprR caseFloatCastR :: RewriteH LCore)
        [ "cast (case s of p -> e) co ==> case s of p -> cast e co" ]        .+ Shallow .+ Commute
    , external "caseFloatLet" (promoteExprR caseFloatLetR :: RewriteH LCore)
        [ "let v = case ec of alt1 -> e1 in e ==> case ec of alt1 -> let v = e1 in e" ] .+ Commute .+ Shallow .+ Strictness
    , external "caseFloat" (promoteExprR caseFloatR :: RewriteH LCore)
        [ "caseFloat = caseFloatApp <+ caseFloatCase <+ caseFloatLet <+ caseFloatCase" ] .+ Commute .+ Shallow .+ Strictness
    , external "caseFloatIn" (promoteExprR caseFloatInR :: RewriteH LCore)
        [ "Float in a Case whatever the context." ]                             .+ Commute .+ Shallow .+ PreCondition
    , external "caseFloatInArgs" (promoteExprR caseFloatInArgsR :: RewriteH LCore)
        [ "Float in a Case whose alternatives are parallel applications of the same function." ] .+ Commute .+ Shallow .+ PreCondition .+ Strictness
    , external "caseReduce" (promoteExprR (caseReduceR True) :: RewriteH LCore)
        [ "Case of Known Constructor"
        , "caseReduceDatacon <+ caseReduceLiteral" ]                     .+ Shallow .+ Eval
    , external "caseReduceDatacon" (promoteExprR (caseReduceDataconR True) :: RewriteH LCore)
        [ "Case of Known Constructor"
        , "case C v1..vn of C w1..wn -> e ==> let { w1 = v1 ; .. ; wn = vn } in e" ]    .+ Shallow .+ Eval
    , external "caseReduceLiteral" (promoteExprR (caseReduceLiteralR True) :: RewriteH LCore)
        [ "Case of Known Constructor"
        , "case L of L -> e ==> e" ]                                         .+ Shallow .+ Eval
    , external "caseReduceUnfold" (promoteExprR (caseReduceUnfoldR True) :: RewriteH LCore)
        [ "Unfold the case scrutinee and then caseReduce." ] .+ Shallow .+ Eval .+ Context
    , external "caseSplit" ((\nm -> findVarT (unOccurrenceName nm) >>= promoteExprR . caseSplitR . varToCoreExpr) :: OccurrenceName -> RewriteH LCore)
        [ "caseSplit 'x"
        , "e ==> case x of C1 vs -> e; C2 vs -> e, where x is free in e" ] .+ Deep .+ Strictness
    , external "caseSplit" (parseCoreExprT >=> promoteR . caseSplitR :: CoreString -> RewriteH LCore)
        [ "caseSplit [| expr |]"
        , "e ==> case expr of C1 vs -> e; C2 vs -> e"] .+ Deep .+ Strictness
    , external "caseSplitInline" ((\nm -> findVarT (unOccurrenceName nm) >>= promoteExprR . caseSplitInlineR . varToCoreExpr) :: OccurrenceName -> RewriteH LCore)
        [ "Like caseSplit, but additionally inlines the matched constructor "
        , "applications for all occurances of the named variable." ] .+ Deep .+ Strictness
    , external "caseSplitInline" (parseCoreExprT >=> promoteExprR . caseSplitInlineR :: CoreString -> RewriteH LCore)
        [ "Like caseSplit, but additionally inlines the matched constructor "
        , "applications for all occurances of the case binder." ] .+ Deep .+ Strictness
    , external "caseIntroSeq" (promoteExprR . caseIntroSeqR . cmpString2Var :: String -> RewriteH LCore)
        [ "Force evaluation of a variable by introducing a case."
        , "caseIntroSeq 'v is is equivalent to adding @(seq v)@ in the source code." ] .+ Shallow .+ Introduce .+ Strictness
    , external "caseElimSeq" (promoteExprR caseElimSeqR :: RewriteH LCore)
        [ "Eliminate a case that corresponds to a pointless seq."  ] .+ Deep .+ Eval .+ Strictness
    , external "caseInlineAlternative" (promoteExprR caseInlineAlternativeR :: RewriteH LCore)
        [ "Inline the case binder as the caseAlternative pattern everywhere in the case alternatives." ] .+ Deep
    , external "caseInlineScrutinee" (promoteExprR caseInlineScrutineeR :: RewriteH LCore)
        [ "Inline the case binder as the case scrutinee everywhere in the case alternatives." ] .+ Deep
    , external "caseMergeAlts" (promoteExprR caseMergeAltsR :: RewriteH LCore)
        [ "Merge all case alternatives into a single default case."
        , "The RHS of each alternative must be the same."
        , "case s of {pat1 -> e ; pat2 -> e ; ... ; patn -> e} ==> case s of {_ -> e}" ]
    , external "caseMergeAltsWithBinder" (promoteExprR caseMergeAltsWithBinderR :: RewriteH LCore)
        [ "A cleverer version of 'mergeCaseAlts' that first attempts to"
        , "abstract out any occurrences of the alternative pattern using the case binder." ] .+ Deep
    , external "caseElim" (promoteExprR caseElimR :: RewriteH LCore)
        [ "case s of w; C vs -> e ==> e if w and vs are not free in e" ]     .+ Shallow .+ Strictness
    , external "caseElimInlineScrutinee" (promoteExprR caseElimInlineScrutineeR :: RewriteH LCore)
        [ "Eliminate a case, inlining any occurrences of the case binder as the scrutinee." ] .+ Deep
    , external "caseElimMergeAlts" (promoteExprR caseElimMergeAltsR :: RewriteH LCore)
        [ "Eliminate a case, merging the case alternatives into a single default alternative",
          "and inlining the case binder as the scrutinee (if possible)." ] .+ Deep
    , external "caseFoldBinder" (promoteExprR caseFoldBinderR :: RewriteH LCore)
        [ "In the case alternatives, fold any occurrences of the case alt patterns to the case binder." ]

      -- HERMIT.API.Dictionary.Local.Cast
    , external "castElim" (promoteExprR castElimR :: RewriteH LCore)
        [ "castElimRefl <+ castElimSym" ] .+ Shallow -- don't include in "Bash", as sub-rewrites are tagged "Bash" already.
    , external "castElimRefl" (promoteExprR castElimReflR :: RewriteH LCore)
        [ "cast e co ==> e ; if co is a reflexive coercion" ] .+ Shallow
    , external "castElimSym" (promoteExprR castElimSymR :: RewriteH LCore)
        [ "removes pairs of symmetric casts" ]                .+ Shallow
    , external "castElimSymPlus" (promoteExprR castElimSymPlusR :: RewriteH LCore)
        [ "removes pairs of symmetric casts possibly separated by let or case forms" ] .+ Deep .+ TODO
    , external "castFloatApp" (promoteExprR castFloatAppR :: RewriteH LCore)
        [ "(cast e (c1 -> c2)) x ==> cast (e (cast x (sym c1))) c2" ] .+ Shallow
    , external "castFloatLam" (promoteExprR castFloatLamR :: RewriteH LCore)
        [ "\\ x::a -> cast x (a -> b) ==> cast (\\x::a -> x) ((a -> a) -> (a -> b))" ] .+ Shallow
    , external "castElimUnsafe" (promoteExprR castElimUnsafeR :: RewriteH LCore)
        [ "removes casts regardless of whether it is safe to do so" ] .+ Shallow .+ Experiment .+ Unsafe .+ TODO

      -- HERMIT.API.Dictionary.Local.Let
    , external "letSubst" (promoteExprR letSubstR :: RewriteH LCore)
        [ "Let substitution: (let x = e1 in e2) ==> (e2[e1/x])"
        , "x must not be free in e1." ]                                         .+ Deep .+ Eval
    , external "letSubstSafe" (promoteExprR letSubstSafeR :: RewriteH LCore)
        [ "Safe let substitution"
        , "let x = e1 in e2, safe to inline without duplicating work ==> e2[e1/x],"
        , "x must not be free in e1." ]                                         .+ Deep .+ Eval
    , external "letNonrecSubstSafe" (promoteExprR letNonRecSubstSafeR :: RewriteH LCore)
        [ "As letSubstSafe, but does not try to convert a recursive let into a nonRecursive let first." ] .+ Deep .+ Eval
    , external "letIntro" (promoteExprR . letIntroR :: String -> RewriteH LCore)
        [ "e => (let v = e in v), name of v is provided" ]                      .+ Shallow .+ Introduce
    , external "letIntroUnfolding" (promoteExprR . letIntroUnfoldingR :: HermitName -> RewriteH LCore)
        [ "e => let f' = defn[f'/f] in e[f'/f], name of f is provided" ]
    , external "letElim" (promoteExprR letElimR :: RewriteH LCore)
        [ "Remove an unused let binding."
        , "(let v = e1 in e2) ==> e2, if v is not free in e1 or e2." ]          .+ Eval .+ Shallow
    , external "letFloatApp" (promoteExprR letFloatAppR :: RewriteH LCore)
        [ "(let v = ev in e) x ==> let v = ev in e x" ]                         .+ Commute .+ Shallow
    , external "letFloatArg" (promoteExprR letFloatArgR :: RewriteH LCore)
        [ "f (let v = ev in e) ==> let v = ev in f e" ]                         .+ Commute .+ Shallow
    , external "letFloatLam" (promoteExprR letFloatLamR :: RewriteH LCore)
        [ "The Full Laziness Transformation"
        , "(\\ v1 -> let v2 = e1 in e2)  ==>  let v2 = e1 in (\\ v1 -> e2), if v1 is not free in e2."
        , "If v1 = v2 then v1 will be alphaRenamed." ]                         .+ Commute .+ Shallow
    , external "letFloatLet" (promoteExprR letFloatLetR :: RewriteH LCore)
        [ "let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e" ] .+ Commute .+ Shallow
    , external "letFloatCase" (promoteExprR letFloatCaseR :: RewriteH LCore)
        [ "case (let v = ev in e) of ... ==> let v = ev in case e of ..." ]     .+ Commute .+ Shallow .+ Eval
--     , external "letFloatCaseAlt" (promoteExprR (letFloatCaseAltR Nothing) :: RewriteH LCore)
--         [ "case s of { ... ; p -> let v = ev in e ; ... } "
--         , "==> let v = ev in case s of { ... ; p -> e ; ... } " ]               .+ Commute .+ Shallow .+ Eval
--     , external "letFloatCaseAlt" (promoteExprR . letFloatCaseAltR . Just :: Int -> RewriteH LCore)
--         [ "Float a let binding from specified alternative."
--         , "case s of { ... ; p -> let v = ev in e ; ... } "
--         , "==> let v = ev in case s of { ... ; p -> e ; ... } " ]               .+ Commute .+ Shallow .+ Eval
    , external "letFloatCast" (promoteExprR letFloatCastR :: RewriteH LCore)
        [ "cast (let bnds in e) co ==> let bnds in cast e co" ]                 .+ Commute .+ Shallow
    , external "letFloatTop" (promoteProgR letFloatTopR :: RewriteH LCore)
        [ "v = (let bds in e) : prog ==> bds : v = e : prog" ]                  .+ Commute .+ Shallow
    , external "letFloat" (promoteProgR letFloatTopR <+ promoteExprR letFloatExprR :: RewriteH LCore)
        [ "Float a Let whatever the context." ]                                 .+ Commute .+ Shallow  -- Don't include in bash, as each subRewrite is tagged "Bash" already.
    , external "letToCase" (promoteExprR letToCaseR :: RewriteH LCore)
        [ "let v = ev in e ==> case ev of v -> e" ]                             .+ Commute .+ Shallow .+ PreCondition
    , external "letFloatIn" (promoteExprR letFloatInR >+> anybuR (promoteExprR letElimR) :: RewriteH LCore)
        [ "FloatIn a let if possible." ]                                        .+ Commute .+ Shallow
    , external "letFloatInApp" ((promoteExprR letFloatInAppR >+> anybuR (promoteExprR letElimR)) :: RewriteH LCore)
        [ "let v = ev in f a ==> (let v = ev in f) (let v = ev in a)" ]         .+ Commute .+ Shallow
    , external "letFloatInCase" ((promoteExprR letFloatInCaseR >+> anybuR (promoteExprR letElimR)) :: RewriteH LCore)
        [ "let v = ev in case s of p -> e ==> case (let v = ev in s) of p -> let v = ev in e"
        , "if v does not shadow a pattern binder in p" ]                        .+ Commute .+ Shallow
    , external "letFloatInLam" ((promoteExprR letFloatInLamR >+> anybuR (promoteExprR letElimR)) :: RewriteH LCore)
        [ "let v = ev in \\ x -> e ==> \\ x -> let v = ev in e"
        , "if v does not shadow x" ]                                            .+ Commute .+ Shallow
     , external "reorderLets" (promoteExprR . reorderNonRecLetsR :: [String] -> RewriteH LCore)
         [ "Reorder a sequence of nested nonRecursive let bindings."
         , "The argument list should contain the letBound variables, in the desired order." ]
    , external "letTuple" (promoteExprR . letTupleR :: String -> RewriteH LCore)
        [ "Combine nested nonRecursive lets into case of a tuple."
        , "E.g. let {v1 = e1 ; v2 = e2 ; v3 = e3} in body ==> case (e1,e2,e3) of {(v1,v2,v3) -> body}" ] .+ Commute
    , external "progBindElim" (promoteProgR progBindElimR :: RewriteH LCore)
        [ "Remove unused topLevel binding(s)."
        , "progBindNonrecElim <+ progBindRecElim" ]                       .+ Eval .+ Shallow
    , external "progBindNonrecElim" (promoteProgR progBindNonRecElimR :: RewriteH LCore)
        [ "Remove unused topLevel binding(s)."
        , "v = e : prog ==> prog, if v is not free in prog and not exported." ] .+ Eval .+ Shallow
    , external "progBindRecElim" (promoteProgR progBindRecElimR :: RewriteH LCore)
        [ "Remove unused topLevel binding(s)."
        , "v+ = e+ : prog ==> v* = e* : prog, where v* is a subset of v+ consisting"
        , "of vs that are free in prog or e+, or exported." ]                   .+ Eval .+ Shallow

      -- HERMIT.API.Dictionary.New
    , external "nonrecIntro" ((\ s str -> promoteCoreR (nonRecIntro s str)) :: String -> CoreString -> RewriteH LCore)
                [ "Introduce a new non-recursive binding.  Only works at Expression or Program nodes."
                , "nonrec-into 'v [| e |]"
                , "body ==> let v = e in body"
                ] .+ Introduce .+ Shallow

      -- ???
    , external "unfoldRemembered" (promoteExprR . unfoldRememberedR Obligation :: LemmaName -> RewriteH LCore)
        [ "Unfold a remembered definition." ] .+ Deep .+ Context

    , external "foldRemembered" (promoteExprR . foldRememberedR Obligation :: LemmaName -> RewriteH LCore)
        [ "Fold a remembered definition." ]                      .+ Context .+ Deep

    , external "foldAnyRemembered" (promoteExprR foldAnyRememberedR :: RewriteH LCore)
        [ "Attempt to fold any of the remembered definitions." ] .+ Context .+ Deep

    , external "wwResultSplit" ((\ abs rep assC -> promoteDefR $ wwSplit (mkWWAssC assC) abs rep)
                                  :: CoreString -> CoreString -> RewriteH LCore -> RewriteH LCore)
                [ "Worker/Wrapper Split (Result Variant)",
                  "For any \"prog :: X -> A\", and given \"abs :: B -> A\" and \"rep :: A -> B\" as arguments,",
                  "and a proof of Assumption C (fix (X->A) (\\ h x -> abs (rep (f h x))) ==> fix (X->A) f), then",
                  "prog = expr  ==>  prog = let f = \\ prog -> expr",
                  "                          in let work = \\ x1 -> rep (f (\\ x2 -> abs (work x2)) x1)",
                  "                              in \\ x0 -> abs (work x0)"
                ] .+ Introduce .+ Context
    , external "wwResultSplitUnsafe" ((\ abs rep -> promoteDefR $ wwSplit Nothing abs rep)
                                       :: CoreString -> CoreString -> RewriteH LCore)
                [ "Unsafe Worker/Wrapper Split (Result Variant)",
                  "For any \"prog :: X -> A\", and given \"abs :: B -> A\" and \"rep :: A -> B\" as arguments, then",
                  "prog = expr  ==>  prog = let f = \\ prog -> expr",
                  "                          in let work = \\ x1 -> rep (f (\\ x2 -> abs (work x2)) x1)",
                  "                              in \\ x0 -> abs (work x0)",
                  "Note: the preCondition \"fix (X->A) (\\ h x -> abs (rep (f h x))) == fix (X->A) f\" is expected to hold."
                ] .+ Introduce .+ Context .+ PreCondition .+ Unsafe
    , external "wwResultSplitStaticArg"  ((\ n is abs rep assC -> promoteDefR $ wwResultSplitStaticArg n is (mkWWAssC assC) abs rep)
                                      :: Int -> [Int] -> CoreString -> CoreString -> RewriteH LCore -> RewriteH LCore)
                [ "Worker/Wrapper Split - Static Argument Variant (Result Variant)",
                  "Perform the static argument transformation on the first n arguments, then perform the worker/wrapper split,",
                  "applying the given abs and rep functions to the specified (by index) static arguments before use."
                ] .+ Introduce .+ Context
    , external "wwResultSplitStaticArgUnsafe" ((\ n is abs rep -> promoteDefR $ wwResultSplitStaticArg n is Nothing abs rep)
                                      :: Int -> [Int] -> CoreString -> CoreString -> RewriteH LCore)
                [ "Unsafe Worker/Wrapper Split - Static Argument Variant (Result Variant)",
                  "Perform the static argument transformation on the first n arguments, then perform the (unsafe) worker/wrapper split,",
                  "applying the given abs and rep functions to the specified (by index) static arguments before use."
                ] .+ Introduce .+ Context .+ PreCondition .+ Unsafe
    , external "wwResultAssAToAssB" (promoteExprR . wwResultAssAimpliesAssB . extractR :: RewriteH LCore -> RewriteH LCore)
                   [ "Convert a proof of worker/wrapper Assumption A into a proof of worker/wrapper Assumption B."
                   ]
    , external "wwResultAssBToAssC" (promoteExprR . wwResultAssBimpliesAssC . extractR :: RewriteH LCore -> RewriteH LCore)
                   [ "Convert a proof of worker/wrapper Assumption B into a proof of worker/wrapper Assumption C."
                   ]
    , external "wwResultAssAToAssC" (promoteExprR . wwResultAssAimpliesAssC . extractR :: RewriteH LCore -> RewriteH LCore)
                   [ "Convert a proof of worker/wrapper Assumption A into a proof of worker/wrapper Assumption C."
                   ]
    , external "wwSplit" ((\ wrap unwrap assC -> promoteDefR $ wwSplit (mkWWAssC assC) wrap unwrap)
                                  :: CoreString -> CoreString -> RewriteH LCore -> RewriteH LCore)
                [ "Worker/Wrapper Split",
                  "For any \"prog :: A\", and given \"wrap :: B -> A\" and \"unwrap :: A -> B\" as arguments,",
                  "and a proof of Assumption C (fix A (\\ a -> wrap (unwrap (f a))) ==> fix A f), then",
                  "prog = expr  ==>  prog = let f = \\ prog -> expr",
                  "                          in let work = unwrap (f (wrap work))",
                  "                              in wrap work"
                ] .+ Introduce .+ Context
    , external "wwSplitUnsafe" ((\ wrap unwrap -> promoteDefR $ wwSplit Nothing wrap unwrap)
                                       :: CoreString -> CoreString -> RewriteH LCore)
                [ "Unsafe Worker/Wrapper Split",
                  "For any \"prog :: A\", and given \"wrap :: B -> A\" and \"unwrap :: A -> B\" as arguments, then",
                  "prog = expr  ==>  prog = let f = \\ prog -> expr",
                  "                          in let work = unwrap (f (wrap work))",
                  "                              in wrap work",
                  "Note: the preCondition \"fix A (wrap . unwrap . f) == fix A f\" is expected to hold."
                ] .+ Introduce .+ Context .+ PreCondition
    , external "wwSplitStaticArg" ((\ n is wrap unwrap assC -> promoteDefR $ wwSplitStaticArg n is (mkWWAssC assC) wrap unwrap)
                                      :: Int -> [Int] -> CoreString -> CoreString -> RewriteH LCore -> RewriteH LCore)
                [ "Worker/Wrapper Split - Static Argument Variant",
                  "Perform the static argument transformation on the first n arguments, then perform the worker/wrapper split,",
                  "applying the given wrap and unwrap functions to the specified (by index) static arguments before use."
                ] .+ Introduce .+ Context
    , external "wwSplitStaticArgUnsafe"  ((\ n is wrap unwrap -> promoteDefR $ wwSplitStaticArg n is Nothing wrap unwrap)
                                      :: Int -> [Int] -> CoreString -> CoreString -> RewriteH LCore)
                [ "Unsafe Worker/Wrapper Split - Static Argument Variant",
                  "Perform the static argument transformation on the first n arguments, then perform the (unsafe) worker/wrapper split,",
                  "applying the given wrap and unwrap functions to the specified (by index) static arguments before use."
                ] .+ Introduce .+ Context .+ PreCondition

    , external "wwAssAToAssB" (promoteExprR . wwAssAimpliesAssB . extractR :: RewriteH LCore -> RewriteH LCore)
                   [ "Convert a proof of worker/wrapper Assumption A into a proof of worker/wrapper Assumption B."
                   ]
    , external "wwAssBToAssC" (promoteExprR . wwAssBimpliesAssC . extractR :: RewriteH LCore -> RewriteH LCore)
                   [ "Convert a proof of worker/wrapper Assumption B into a proof of worker/wrapper Assumption C."
                   ]
    , external "wwAssAToAssC" (promoteExprR . wwAssAimpliesAssC . extractR :: RewriteH LCore -> RewriteH LCore)
                   [ "Convert a proof of worker/wrapper Assumption A into a proof of worker/wrapper Assumption C."
                   ]
   , external "split1Beta" (\ nm absC -> promoteExprR . parse2BeforeT (split1BetaR Obligation nm) absC :: CoreString -> RewriteH LCore)
        [ "split1Beta <name> <abs expression> <rep expression>"
        , "Perform worker/wrapper split with condition 1-beta."
        , "Given lemma name argument is used as prefix to two introduced lemmas."
        , "  <name>-assumption: unproven lemma for w/w assumption C."
        , "  <name>-fusion: assumed lemma for w/w fusion."
        ]
   , external "split2Beta" (\ nm absC -> promoteExprR . parse2BeforeT (split2BetaR Obligation nm) absC :: CoreString -> RewriteH LCore)
        [ "split2Beta <name> <abs expression> <rep expression>"
        , "Perform worker/wrapper split with condition 2-beta."
        , "Given lemma name argument is used as prefix to two introduced lemmas."
        , "  <name>-assumption: unproven lemma for w/w assumption C."
        , "  <name>-fusion: assumed lemma for w/w fusion."
        ]

    , external "betaReducePlus" (promoteExprR betaReducePlusR :: RewriteH LCore)
        [ "Perform one or more beta-reductions."]                               .+ Eval .+ Shallow
    , external "unfoldSaturated" (promoteExprR unfoldSaturatedR :: RewriteH LCore)
        [ "Unfold a definition only if the function is fully applied." ] .+ Deep .+ Context
    , external "specialize" (promoteExprR specializeR :: RewriteH LCore)
        [ "Specialize an application to its type and coercion arguments." ] .+ Deep .+ Context

    , external "replaceCurrentExprWithUndefined" (promoteExprR replaceCurrentExprWithUndefinedR :: RewriteH LCore)
        [ "Set the current expression to \"undefined\"."
        ] .+ Shallow .+ Context .+ Unsafe
    , external "replaceIdWithUndefined" (promoteCoreR . replaceIdWithUndefined :: HermitName -> RewriteH LCore)
        [ "Replace the specified identifier with \"undefined\"."
        ] .+ Deep .+ Context .+ Unsafe
    , external "errorToUndefined" (promoteExprR errorToUndefinedR :: RewriteH LCore)
        [ "error ty string  ==>  undefined ty"
        ] .+ Shallow .+ Context
    , external "undefinedExpr" (promoteExprR undefinedExprR :: RewriteH LCore)
        [ "undefined-app <+ undefined-lam <+ undefined-let <+ undefined-cast <+ undefined-tick <+ undefined-case"
        ] .+ Eval .+ Shallow .+ Context
    , external "undefinedApp"  (promoteExprR undefinedAppR :: RewriteH LCore)
        [ "(undefined ty1) e  ==>  undefined ty2"
        ] .+ Eval .+ Shallow .+ Context
    , external "undefinedLam" (promoteExprR undefinedLamR :: RewriteH LCore)
        [ "(\\ v -> undefined ty1)  ==>  undefined ty2   (where v is not a 'TyVar')"
        ] .+ Eval .+ Shallow .+ Context
    , external "undefinedLet" (promoteExprR undefinedLetR :: RewriteH LCore)
        [ "let bds in (undefined ty)  ==>  undefined ty"
        ] .+ Eval .+ Shallow .+ Context
    , external "undefinedCase" (promoteExprR undefinedCaseR :: RewriteH LCore)
        [ "case (undefined ty) of alts  ==>  undefined ty"
        , "OR"
        , "case e of {pat_1 -> undefined ty ; pat_2 -> undefined ty ; ... ; pat_n -> undefined ty} ==> undefined ty"
        ] .+ Eval .+ Shallow .+ Context
    , external "undefinedCast" (promoteExprR undefinedCastR :: RewriteH LCore)
        [ "Cast (undefined ty1) co  ==>  undefined ty2"
        ] .+ Eval .+ Shallow .+ Context
    , external "undefinedTick" (promoteExprR undefinedTickR :: RewriteH LCore)
        [ "Tick tick (undefined ty1)  ==>  undefined ty1"
        ] .+ Eval .+ Shallow .+ Context

    , external "foldRule" (promoteExprR . foldRuleR Obligation :: RuleName -> RewriteH LCore)
        [ "Apply a named GHC rule right-to-left." ] .+ Shallow
    , external "foldRules" (promoteExprR . foldRulesR Obligation :: [RuleName] -> RewriteH LCore)
        [ "Apply named GHC rules right-to-left, succeed if any of the rules succeed." ] .+ Shallow
    , external "unfoldRule" (promoteExprR . unfoldRuleR Obligation :: RuleName -> RewriteH LCore)
        [ "Apply a named GHC rule left-to-right." ] .+ Shallow
    , external "unfoldRuleUnsafe" (promoteExprR . unfoldRuleR UnsafeUsed :: RuleName -> RewriteH LCore)
        [ "Apply a named GHC rule left-to-right." ] .+ Shallow .+ Unsafe
    , external "unfoldRules" (promoteExprR . unfoldRulesR Obligation :: [RuleName] -> RewriteH LCore)
        [ "Apply named GHC rules left-to-right, succeed if any of the rules succeed" ] .+ Shallow
    , external "unfoldRulesUnsafe" (promoteExprR . unfoldRulesR UnsafeUsed :: [RuleName] -> RewriteH LCore)
        [ "Apply named GHC rules left-to-right, succeed if any of the rules succeed" ] .+ Shallow .+ Unsafe
    , external "specConstr" (promoteModGutsR specConstrR :: RewriteH LCore)
        [ "Run GHC's SpecConstr pass, which performs call pattern specialization."] .+ Deep
    , external "specialise" (promoteModGutsR specialiseR :: RewriteH LCore)
        [ "Run GHC's specialisation pass, which performs type and dictionary specialisation."] .+ Deep
    , external "mergeQuantifiers" (\n1 n2 -> promoteR (mergeQuantifiersR (cmpHN2Var n1) (cmpHN2Var n2)) :: RewriteH LCore)
        [ "Merge quantifiers from two clauses if they have the same type."
        , "Example:"
        , "(forall (x::Int). foo x = x) ^ (forall (y::Int). bar y y = 5)"
        , "merge-quantifiers 'x 'y"
        , "forall (x::Int). (foo x = x) ^ (bar x x = 5)"
        , "Note: if only one quantifier matches, it will be floated if possible." ]
    , external "floatLeft" (\n1 -> promoteR (mergeQuantifiersR (cmpHN2Var n1) (const False)) :: RewriteH LCore)
        [ "Float quantifier out of left-hand side." ]
    , external "foldRight" (\n1 -> promoteR (mergeQuantifiersR (const False) (cmpHN2Var n1)) :: RewriteH LCore)
        [ "Float quantifier out of right-hand side." ]
    , external "lemmaForward" (forwardT . promoteExprBiR . lemmaBiR Obligation :: LemmaName -> RewriteH LCore)
        [ "Generate a rewrite from a lemma, left-to-right." ]
    , external "lemmaBackward" (backwardT . promoteExprBiR . lemmaBiR Obligation :: LemmaName -> RewriteH LCore)
        [ "Generate a rewrite from a lemma, right-to-left." ]
    , external "lemma-consequent" (promoteClauseR . lemmaConsequentR Obligation :: LemmaName -> RewriteH LCore)
        [ "Match the current lemma with the consequent of an implication lemma."
        , "Upon success, replaces with antecedent of the implication, properly instantiated." ]
    , external "lemmaLhsIntro" (promoteCoreR . lemmaLhsIntroR :: LemmaName -> RewriteH LCore)
        [ "Introduce the LHS of a lemma as a non-recursive binding, in either an expression or a program."
        , "body ==> let v = lhs in body" ] .+ Introduce .+ Shallow
    , external "lemmaRhsIntro" (promoteCoreR . lemmaRhsIntroR :: LemmaName -> RewriteH LCore)
        [ "Introduce the RHS of a lemma as a non-recursive binding, in either an expression or a program."
        , "body ==> let v = rhs in body" ] .+ Introduce .+ Shallow
    , external "instDictionaries" (promoteClauseR instantiateDictsR :: RewriteH LCore)
        [ "Instantiate all of the universally quantified dictionaries of the given lemma." ]
    , external "reflexivity" (promoteClauseR (forallR idR reflexivityR <+ reflexivityR) :: RewriteH LCore)
        [ "Rewrite alpha-equivalence to true." ]
    , external "simplifyLemma" (simplifyClauseR :: RewriteH LCore)
        [ "Reduce a proof by applying reflexivity and logical operator identities." ]
    , external "splitAntecedent" (promoteClauseR splitAntecedentR :: RewriteH LCore)
        [ "Split an implication of the form (q1 ^ q2) => q3 into q1 => (q2 => q3)" ]
    , external "lemma" (promoteClauseR . lemmaR Obligation :: LemmaName -> RewriteH LCore)
        [ "Rewrite clause to true using given lemma." ]
    , external "lemmaUnsafe" (promoteClauseR . lemmaR UnsafeUsed :: LemmaName -> RewriteH LCore)
        [ "Rewrite clause to true using given lemma." ] .+ Unsafe
    ]
    where
      mkWWAssC :: RewriteH LCore -> Maybe WWAssumption
      mkWWAssC r = Just (WWAssumption C (extractR r))

instance External (RewriteH LCoreTC) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.Debug
      external "trace" (traceR :: String -> RewriteH LCoreTC)
        [ "give a sideEffect message as output when processing this command" ]
    , external "observe" (observeR :: String -> RewriteH LCoreTC)
        [ "give a sideEffect message as output, and observe the value being processed" ]
    , external "observeFailure" (observeFailureR :: String -> RewriteH LCoreTC -> RewriteH LCoreTC)
        [ "give a sideEffect message if the rewrite fails, including the failing input" ]
    , external "bracket" (bracketR :: String -> RewriteH LCoreTC -> RewriteH LCoreTC)
        [ "if given rewrite succeeds, see its input and output" ]

      -- HERMIT.API.Dictionary.KURE
    , external "id"         (idR :: RewriteH LCoreTC)
        [ "Perform an identity rewrite."] .+ Shallow
--     , external "focus"      ((\p -> hfocusR (return p)) :: LocalPathH -> RewriteH LCoreTC -> RewriteH LCoreTC)
--         [ "Apply a rewrite to a focal point."] .+ Navigation .+ Deep
--     , external "focus"      (hfocusR :: TransformH LCoreTC LocalPathH -> RewriteH LCoreTC -> RewriteH LCoreTC)
--         [ "Apply a rewrite to a focal point."] .+ Navigation .+ Deep
    , external ">>>"        ((>>>) :: RewriteH LCoreTC -> RewriteH LCoreTC -> RewriteH LCoreTC)
        [ "Compose rewrites, requiring both to succeed." ]
    , external "promote"    (promoteR :: RewriteH LCore -> RewriteH LCoreTC)
        [ "Promote a RewriteCore to a RewriteCoreTC" ]
    , external "between"    (betweenR :: Int -> Int -> RewriteH LCoreTC -> RewriteH LCoreTC)
        [ "between x y rr -> perform rr at least x times and at most y times." ]
--    , external "atPath"     (flip hfocusT idR :: TransformH LCoreTC LocalPathH -> TransformH LCoreTC LCoreTC)
--        [ "return the expression found at the given path" ]

    , external "unshadowQuantified" (promoteClauseR unshadowClauseR :: RewriteH LCoreTC)
        [ "Unshadow a quantified clause." ]
    ]

-------------------------------------------------------------------------------

instance External (TransformH LCore LocalPathH) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.Navigation
--       external "consider" (considerConstructT :: Considerable -> TransformH LCore LocalPathH)
--         [ "consider <c> focuses on the first construct <c>.", recognizedConsiderables ]
      external "arg" (promoteExprT . nthArgPath :: Int -> TransformH LCore LocalPathH)
        [ "arg n focuses on the (n-1)th argument of a nested application." ]
    , external "lamsBody" (promoteExprT lamsBodyT :: TransformH LCore LocalPathH)
        [ "Descend into the body after a sequence of lambdas." ]
    , external "letsBody" (promoteExprT letsBodyT :: TransformH LCore LocalPathH)
        [ "Descend into the body after a sequence of let bindings." ]
    , external "progEnd" (promoteModGutsT gutsProgEndT <+ promoteProgT progEndT :: TransformH LCore LocalPathH)
        [ "Descend to the end of a program." ]
{-    , external "parentOf" (parentOfT :: TransformH LCore LocalPathH -> TransformH LCore LocalPathH)
        [ "Focus on the parent of another focal point." ]  -}
    ]

instance External (TransformH LCoreTC LocalPathH) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.Navigation
      external "rhsOf" (rhsOfT . mkRhsOfPred :: RhsOfName -> TransformH LCoreTC LocalPathH)
        [ "Find the path to the RHS of the binding of the named variable." ]
    , external "bindingGroupOf" (bindingGroupOfT . cmpString2Var :: String -> TransformH LCoreTC LocalPathH)
        [ "Find the path to the binding group of the named variable." ]
    , external "bindingOf" (bindingOfT . mkBindingPred :: BindingName -> TransformH LCoreTC LocalPathH)
        [ "Find the path to the binding of the named variable." ]
    , external "occurrenceOf" (occurrenceOfT . mkOccPred :: OccurrenceName -> TransformH LCoreTC LocalPathH)
        [ "Find the path to the first occurrence of the named variable." ]
    , external "applicationOf" (applicationOfT . mkOccPred :: OccurrenceName -> TransformH LCoreTC LocalPathH)
        [ "Find the path to the first application of the named variable." ]
--     , external "parentOf" (parentOfT :: TransformH LCoreTC LocalPathH -> TransformH LCoreTC LocalPathH)
--         [ "Focus on the parent of another focal point." ]
    ]

instance External (TransformH LCoreTC String) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.GHC
      external "lintExpr" (promoteExprT lintExprT :: TransformH LCoreTC String)
        [ "Runs GHC's Core Lint, which typechecks the current expression."
        , "Note: this can miss several things that a whole module core lint will find."
        , "For instance, running this on the RHS of a binding, the type of the RHS will"
        , "not be checked against the type of the binding. Running on the whole let expression"
        , "will catch that however."] .+ Deep .+ Debug .+ Query
    , external "lintModule" (promoteModGutsT lintModuleT :: TransformH LCoreTC String)
        [ "Runs GHC's Core Lint, which typechecks the current module."] .+ Deep .+ Debug .+ Query
    , external "lint" (promoteT lintClauseT :: TransformH LCoreTC String)
        [ "Lint check a clause." ]

      -- HERMIT.API.Dictionary.KURE
-- --     , external "focus"      (hfocusT :: TransformH LCoreTC LocalPathH -> TransformH LCoreTC String -> TransformH LCoreTC String)
-- --         [ "Apply a query at a focal point."] .+ Navigation .+ Deep
-- --     , external "focus"      ((\p -> hfocusT (return p)) :: LocalPathH -> TransformH LCoreTC String -> TransformH LCoreTC String)
-- --         [ "Apply a query at a focal point."] .+ Navigation .+ Deep

    , external "showRules" (rulesHelpListT :: TransformH LCoreTC String)
        [ "List all the rules in scope." ] .+ Query

      -- HERMIT.API.Dictionary.Query
    , external "info" (promoteCoreTCT infoT :: TransformH LCoreTC String)
        [ "Display information about the current node." ] .+ Query
    ]

instance External (TransformH LCore ()) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.GHC
      external "injectDependency" (promoteModGutsT . injectDependencyT . mkModuleName :: String -> TransformH LCore ())
        [ "Inject a dependency on the given module." ]

      -- HERMIT.API.Dictionary.KURE
    , external "<+"         ((<+) :: TransformH LCore () -> TransformH LCore () -> TransformH LCore ())
        [ "Perform the first check, and then, if it fails, perform the second check." ]
    , external "success"    (successT :: TransformH LCore ())
        [ "An always succeeding translation." ]
    , external "not_"        (notM :: TransformH LCore () -> TransformH LCore ())
       [ "Cause a failing check to succeed, a succeeding check to fail."  ] .+ Predicate

      -- HERMIT.API.Dictionary.New
    , external "var" (promoteExprT . isVar :: String -> TransformH LCore ())
                [ "var '<v> returns successfully for variable v, and fails otherwise."
                , "Useful in combination with \"when\", as in: when (var v) r"
                ] .+ Predicate

      -- ???
    , external "remember" (promoteCoreT . rememberR :: LemmaName -> TransformH LCore ()) -- Done not smell right (return ()?)
        [ "Remember the current binding, allowing it to be folded/unfolded in the future." ] .+ Context

    , external "wwResultGenerateFusion" (wwResultGenerateFusionT . mkWWAssC :: RewriteH LCore -> TransformH LCore ())
                   [ "Given a proof of Assumption C (fix (X->A) (\\ h x -> abs (rep (f h x))) ==> fix (X->A) f), then",
                     "execute this command on \"work = \\ x1 -> rep (f (\\ x2 -> abs (work x2)) x1)\" to enable the \"wwResultFusion\" rule thereafter.",
                     "Note that this is performed automatically as part of \"wwResultSplit\"."
                   ] .+ Experiment .+ TODO
    , external "wwResultGenerateFusionUnsafe" (wwResultGenerateFusionT Nothing :: TransformH LCore ())
                   [ "Execute this command on \"work = \\ x1 -> rep (f (\\ x2 -> abs (work x2)) x1)\" to enable the \"wwFusion\" rule thereafter.",
                     "The precondition \"fix (X->A) (\\ h x -> abs (rep (f h x))) == fix (X->A) f\" is expected to hold.",
                     "Note that this is performed automatically as part of \"wwResultSplit\"."
                   ] .+ Experiment .+ TODO
     , external "wwGenerateFusion" (wwGenerateFusionT . mkWWAssC :: RewriteH LCore -> TransformH LCore ())
                   [ "Given a proof of Assumption C (fix A (\\ a -> wrap (unwrap (f a))) ==> fix A f), then",
                     "execute this command on \"work = unwrap (f (wrap work))\" to enable the \"wwFusion\" rule thereafter.",
                     "Note that this is performed automatically as part of \"wwSplit\"."
                   ] .+ Experiment .+ TODO
    , external "wwGenerateFusionUnsafe" (wwGenerateFusionT Nothing :: TransformH LCore ())
                   [ "Execute this command on \"work = unwrap (f (wrap work))\" to enable the \"wwFusion\" rule thereafter.",
                     "The precondition \"fix A (wrap . unwrap . f) == fix A f\" is expected to hold.",
                     "Note that this is performed automatically as part of \"wwSplit\"."
                   ] .+ Experiment .+ TODO

   , external "introWWAssumptionA"
      (\nm absC repC -> do
            q <- parse2BeforeT assumptionAClauseT absC repC
            insertLemmaT nm $ Lemma q NotProven NotUsed :: TransformH LCore ())
        [ "Introduce a lemma for worker/wrapper assumption A"
        , "using given abs and rep functions." ]
   , external "introWWAssumptionB"
      (\nm absC repC bodyC -> do
            q <- parse3BeforeT assumptionBClauseT absC repC bodyC
            insertLemmaT nm $ Lemma q NotProven NotUsed :: TransformH LCore ())
        [ "Introduce a lemma for worker/wrapper assumption B"
        , "using given abs, rep, and body functions." ]
   , external "introWWAssumptionC"
      (\nm absC repC bodyC -> do
            q <- parse3BeforeT assumptionCClauseT absC repC bodyC
            insertLemmaT nm $ Lemma q NotProven NotUsed :: TransformH LCore ())
        [ "Introduce a lemma for worker/wrapper assumption C"
        , "using given abs, rep, and body functions." ]
    , external "isUndefinedVal" (promoteExprT isUndefinedValT :: TransformH LCore ())
        [ "Succeed if the current expression is an undefined value."
        ] .+ Shallow .+ Context .+ Predicate
    , external "imply" (\n1 n2 n3 -> implyLemmasT n1 n2 n3 :: TransformH LCore ())
        [ "imply new-name antecedent-name consequent-name" ]
    , external "instLemma" (\ nm v cs -> modifyLemmaT nm id (instantiateClauseVarR (cmpHN2Var v) cs) id id :: TransformH LCore ())
        [ "Instantiate one of the universally quantified variables of the given lemma,"
        , "with the given Core expression, creating a new lemma. Instantiating an"
        , "already proven lemma will result in the new lemma being considered proven." ]
   , external "copyLemma" (\ nm newName -> modifyLemmaT nm (const newName) idR id id :: TransformH LCore ())
        [ "Copy a given lemma, with a new name." ]
        , external "modifyLemma" ((\ nm rr -> modifyLemmaT nm id (extractR rr) (const NotProven) (const NotUsed)) :: LemmaName -> RewriteH LCore -> TransformH LCore ())
        [ "Modify a given lemma. Resets proven status to Not Proven and used status to Not Used." ]
    , external "conjunct" (\n1 n2 n3 -> conjunctLemmasT n1 n2 n3 :: TransformH LCore ())
        [ "conjunct new-name lhs-name rhs-name" ]
    , external "disjunct" (\n1 n2 n3 -> disjunctLemmasT n1 n2 n3 :: TransformH LCore ())
        [ "disjunct new-name lhs-name rhs-name" ]
   ]
    where
      mkWWAssC :: RewriteH LCore -> Maybe WWAssumption
      mkWWAssC r = Just (WWAssumption C (extractR r))

instance External (TransformH LCore String) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.GHC
--       external "loadLemmaLibrary" (flip loadLemmaLibraryT Nothing :: HermitName -> TransformH LCore String)
--         [ "Dynamically load a library of lemmas." ]
--     , external "loadLemmaLibrary" ((\nm -> loadLemmaLibraryT nm . Just) :: HermitName -> LemmaName -> TransformH LCore String)
--         [ "Dynamically load a specific lemma from a library of lemmas." ]

      -- HERMIT.API.Dictionary.KURE
--     , external "focus"      (hfocusT :: TransformH LCore LocalPathH -> TransformH LCore String -> TransformH LCore String)
--         [ "Apply a query at a focal point."] .+ Navigation .+ Deep
--     , external "focus"      ((\p -> hfocusT (return p)) :: LocalPathH -> TransformH LCore String -> TransformH LCore String)
--         [ "Apply a query at a focal point."] .+ Navigation .+ Deep
      external "test"       (testQuery :: RewriteH LCore -> TransformH LCore String)
        [ "Determine if a rewrite could be successfully applied." ]
--     , external "extract"    (extractT :: TransformH LCoreTC String -> TransformH LCore String)
--         [ "Extract a TransformLCoreString from a TransformLCoreTCString" ]

    , external "queryLemma" ((\ nm t -> getLemmaByNameT nm >>> arr lemmaC >>> extractT t) :: LemmaName -> TransformH LCore String -> TransformH LCore String)
        [ "Apply a transformation to a lemma, returning the result." ]
    ]

instance External (TransformH LCore DocH) where
  parseExternals =
    [ external "ruleToLemma" ((\pp nm -> ruleToLemmaT nm >> liftPrettyH (pOptions pp) (showLemmaT (fromString (show nm)) pp)) :: PrettyPrinter -> RuleName -> TransformH LCore DocH)
        [ "Create a lemma from a GHC RULE." ]
    ]

instance External (TransformH LCoreTC DocH) where
  parseExternals =
    [ external "showRule" (ruleHelpT :: PrettyPrinter -> RuleName -> TransformH LCoreTC DocH)
        [ "Display details on the named rule." ] .+ Query
    ]

instance External (TransformH LCoreTC ()) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.Query
      external "compareBoundIds" (compareBoundIds :: HermitName -> HermitName -> TransformH LCoreTC ())
        [ "Compare the definitions of two in-scope identifiers for alpha equality."] .+ Query .+ Predicate
    , external "compareCoreAt" (compareCoreAtT ::  TransformH LCoreTC LocalPathH -> TransformH LCoreTC LocalPathH -> TransformH LCoreTC ())
        [ "Compare the core fragments at the end of the given paths for alpha-equality."] .+ Query .+ Predicate
    ]

instance External (PrettyH LCore) where
  parseExternals =
    [ external "showLemma"((\pp n -> showLemmaT n pp) :: PrettyPrinter -> LemmaName -> PrettyH LCore)
        [ "Display a lemma." ]
    ]


{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Transform where

import           Control.Arrow

import           Data.Proxy

import           HERMIT.Context
import           HERMIT.Dictionary
import           HERMIT.External (CoreString)
import           HERMIT.GHC
import           HERMIT.Kure
import           HERMIT.Lemma
import           HERMIT.Name
import           HERMIT.ParserCore

import           HERMIT.Server.Parser.Name ()
import           HERMIT.Server.Parser.String ()
import           HERMIT.Server.Parser.Utils

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
                  "Note: the pre-condition \"fix A (\\ a -> wrap (unwrap (f a))) == fix A f\" is expected to hold."
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
                  "Note: you are required to have previously executed the command \"ww-generate-fusion\" on the definition",
                  "      work = \\ x1 -> rep (f (\\ x2 -> abs (work x2)) x1)"
                ] .+ Introduce .+ Context .+ PreCondition .+ TODO
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
        [ "Rename all top-level identifiers in the program."]
    ,  external "unshadow" (promoteCoreR unshadowR :: RewriteH LCore)
        [ "Rename local variables with manifestly unique names (x, x0, x1, ...)."]

      -- HERMIT.API.Dictionary.Composite
    , external "unfoldBasicCombinator" (promoteExprR unfoldBasicCombinatorR :: RewriteH LCore)
        [ "Unfold the current expression if it is one of the basic combinators:"
        , "($), (.), id, flip, const, fst, snd, curry, and uncurry." ]
    , external "simplify" (simplifyR :: RewriteH LCore)
        [ "innermost (unfold-basic-combinator <+ beta-reduce-plus <+ safe-let-subst <+ case-reduce <+ let-elim)" ]
    , external "bash" (bashR :: RewriteH LCore)
        bashHelp .+ Eval .+ Deep .+ Loop
    , external "smash" (smashR :: RewriteH LCore)
        smashHelp .+ Eval .+ Deep .+ Loop .+ Experiment
--     , external "bashExtendedWith" (bashExtendedWithR :: [RewriteH LCore] -> RewriteH LCore)
--         [ "Run \"bash\" extended with additional rewrites.",
--           "Note: be sure that the new rewrite either fails or makes progress, else this may loop."
--         ] .+ Eval .+ Deep .+ Loop
--     , external "smashExtendedWith" (smashExtendedWithR :: [RewriteH LCore] -> RewriteH LCore)
--         [ "Run \"smash\" extended with additional rewrites.",
--           "Note: be sure that the new rewrite either fails or makes progress, else this may loop."
--         ] .+ Eval .+ Deep .+ Loop
    , external "bashDebug" (bashDebugR :: RewriteH LCore)
        [ "verbose bash - most useful with set-auto-corelint True" ] .+ Eval .+ Deep .+ Loop

      -- HERMIT.API.Dictionary.FixPoint
    , external "fixIntro" (promoteCoreR fixIntroR :: RewriteH LCore)
        [ "rewrite a function binding into a non-recursive binding using fix" ] .+ Introduce .+ Context

      -- HERMIT.API.Dictionary.Fold
    , external "fold" (promoteExprR . foldR :: HermitName -> RewriteH LCore)
        [ "fold a definition"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        , ""
        , "5 + 5 + 6"
        , "any-bu (fold 'double)"
        , "double 5 + 6"
        , ""
        , "Note: due to associativity, if you wanted to fold 5 + 6 + 6, "
        , "you first need to apply an associativity rewrite." ]  .+ Context .+ Deep

      -- HERMIT.API.Dictionary.Function
    , external "static-arg" (promoteDefR staticArgR :: RewriteH LCore)
        [ "perform the static argument transformation on a recursive function." ]
    , external "static-arg-types" (promoteDefR staticArgTypesR :: RewriteH LCore)
        [ "perform the static argument transformation on a recursive function, only transforming type arguments." ]
--     , external "static-arg-pos" (promoteDefR . staticArgPosR :: [Int] -> RewriteH LCore)
--         [ "perform the static argument transformation on a recursive function, only transforming the arguments specified (by index)." ]

      -- HERMIT.API.Dictionary.GHC
    , external "deshadowProg" (promoteProgR deShadowProgR :: RewriteH LCore)
        [ "Deshadow a program." ] .+ Deep
    , external "dezombify" (promoteExprR dezombifyR :: RewriteH LCore)
        [ "Zap the occurrence information in the current identifer if it is a zombie."] .+ Shallow
    , external "occurrenceAnalysis" (occurrenceAnalysisR :: RewriteH LCore)
        [ "Perform dependency analysis on all sub-expressions; simplifying and updating identifer info."] .+ Deep

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
        [ "if v is a case binder, replace (Var v) with the bound case-alternative pattern." ] .+ Eval .+ Deep

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
        [ "Promote a rewrite to operate over an entire tree in bottom-up order, requiring success at every node." ] .+ Deep
    , external "allTD"     (alltdR :: RewriteH LCore -> RewriteH LCore)
        [ "Promote a rewrite to operate over an entire tree in top-down order, requiring success at every node." ] .+ Deep
    , external "allDU"     (allduR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite twice, in a top-down and bottom-up way, using one single tree traversal,",
          "succeeding if they all succeed."] .+ Deep
    , external "anyBU"     (anybuR :: RewriteH LCore -> RewriteH LCore)
        [ "Promote a rewrite to operate over an entire tree in bottom-up order, requiring success for at least one node." ] .+ Deep
    , external "anyTD"     (anytdR :: RewriteH LCore -> RewriteH LCore)
        [ "Promote a rewrite to operate over an entire tree in top-down order, requiring success for at least one node." ] .+ Deep
    , external "anyDU"     (anyduR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite twice, in a top-down and bottom-up way, using one single tree traversal,",
          "succeeding if any succeed."] .+ Deep
    , external "oneTD"     (onetdR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to the first node (in a top-down order) for which it can succeed." ] .+ Deep
    , external "oneBU"     (onebuR :: RewriteH LCore -> RewriteH LCore)
        [ "Apply a rewrite to the first node (in a bottom-up order) for which it can succeed." ] .+ Deep
    , external "pruneTD"   (prunetdR :: RewriteH LCore -> RewriteH LCore)
        [ "Attempt to apply a rewrite in a top-down manner, prunning at successful rewrites." ] .+ Deep
    , external "innermost"  (innermostR :: RewriteH LCore -> RewriteH LCore)
        [ "A fixed-point traveral, starting with the innermost term." ] .+ Deep .+ Loop
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
        [ "any-call (.. unfold command ..) applies an unfold command to all applications."
        , "Preference is given to applications with more arguments." ] .+ Deep
--     , external "extract"    (extractR :: RewriteH LCoreTC -> RewriteH LCore)
--         [ "Extract a RewriteCore from a RewriteCoreTC" ]
--    , external "atPath"     (flip hfocusT idR :: TransformH LCore LocalPathH -> TransformH LCore LCore)
--        [ "return the expression found at the given path" ]
--    , external "atPath"     (extractT . flip hfocusT projectT :: TransformH LCoreTC LocalPathH -> TransformH LCore LCore)
--        [ "return the expression found at the given path" ]

      -- ???
    , external "unfoldRemembered" (promoteExprR . unfoldRememberedR Obligation :: LemmaName -> RewriteH LCore)
        [ "Unfold a remembered definition." ] .+ Deep .+ Context

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
                  "Note: the pre-condition \"fix (X->A) (\\ h x -> abs (rep (f h x))) == fix (X->A) f\" is expected to hold."
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
    ]
    where
      mkWWAssC :: RewriteH LCore -> Maybe WWAssumption
      mkWWAssC r = Just (WWAssumption C (extractR r))

instance External (RewriteH LCoreTC) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.Debug
      external "trace" (traceR :: String -> RewriteH LCoreTC)
        [ "give a side-effect message as output when processing this command" ]
    , external "observe" (observeR :: String -> RewriteH LCoreTC)
        [ "give a side-effect message as output, and observe the value being processed" ]
    , external "observeFailure" (observeFailureR :: String -> RewriteH LCoreTC -> RewriteH LCoreTC)
        [ "give a side-effect message if the rewrite fails, including the failing input" ]
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


    ]

-------------------------------------------------------------------------------

instance External (TransformH LCoreTC LocalPathH) where
  parseExternals =
    [
      -- ???
      external "rhsOf"      (rhsOfT . mkRhsOfPred       :: RhsOfName -> TransformH LCoreTC LocalPathH)
            [ "Find the path to the RHS of the binding of the named variable." ]
    , external "bindingOf" (bindingOfT . mkBindingPred :: BindingName -> TransformH LCoreTC LocalPathH)
            [ "Find the path to the binding of the named variable." ]
    ]

instance External (TransformH LCoreTC String) where
  parseExternals =
    [
      -- HERMIT.API.Dictionary.GHC
      external "lintExpr" (promoteExprT lintExprT :: TransformH LCoreTC String)
        [ "Runs GHC's Core Lint, which typechecks the current expression."
        , "Note: this can miss several things that a whole-module core lint will find."
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

      -- ???
    , external "remember" (promoteCoreT . rememberR :: LemmaName -> TransformH LCore ()) -- Done not smell right (return ()?)
        [ "Remember the current binding, allowing it to be folded/unfolded in the future." ] .+ Context

    , external "wwResultGenerateFusion" (wwResultGenerateFusionT . mkWWAssC :: RewriteH LCore -> TransformH LCore ())
                   [ "Given a proof of Assumption C (fix (X->A) (\\ h x -> abs (rep (f h x))) ==> fix (X->A) f), then",
                     "execute this command on \"work = \\ x1 -> rep (f (\\ x2 -> abs (work x2)) x1)\" to enable the \"ww-result-fusion\" rule thereafter.",
                     "Note that this is performed automatically as part of \"ww-result-split\"."
                   ] .+ Experiment .+ TODO
    , external "wwResultGenerateFusionUnsafe" (wwResultGenerateFusionT Nothing :: TransformH LCore ())
                   [ "Execute this command on \"work = \\ x1 -> rep (f (\\ x2 -> abs (work x2)) x1)\" to enable the \"ww-fusion\" rule thereafter.",
                     "The precondition \"fix (X->A) (\\ h x -> abs (rep (f h x))) == fix (X->A) f\" is expected to hold.",
                     "Note that this is performed automatically as part of \"ww-result-split\"."
                   ] .+ Experiment .+ TODO
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
    ]


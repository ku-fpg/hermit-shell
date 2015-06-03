{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Rewrite where

import Control.Arrow

import Data.Proxy

import HERMIT.Dictionary
import HERMIT.Kure
import HERMIT.Lemma
import HERMIT.Name

import HERMIT.Server.Parser.Name ()
import HERMIT.Server.Parser.Utils

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
--     , external "id_"         (idR :: RewriteH LCore)
--         [ "Perform an identity rewrite."] .+ Shallow
--     , external "fail_"       (fail :: String -> RewriteH LCore)
--         [ "A failing rewrite."]
--     , external "<+"         ((<+) :: RewriteH LCore -> RewriteH LCore -> RewriteH LCore)
--         [ "Perform the first rewrite, and then, if it fails, perform the second rewrite." ]
--     , external ">>>"        ((>>>) :: RewriteH LCore -> RewriteH LCore -> RewriteH LCore)
--         [ "Compose rewrites, requiring both to succeed." ]
--     , external ">+>"        ((>+>) :: RewriteH LCore -> RewriteH LCore -> RewriteH LCore)
--         [ "Compose rewrites, allowing one to fail." ]
--     , external "try"        (tryR :: RewriteH LCore -> RewriteH LCore)
--         [ "Try a rewrite, and perform the identity if the rewrite fails." ]
--     , external "repeat"     (repeatR :: RewriteH LCore -> RewriteH LCore)
--         [ "Repeat a rewrite until it would fail." ] .+ Loop
--     , external "replicate"  ((\ n -> andR . replicate n)  :: Int -> RewriteH LCore -> RewriteH LCore)
--         [ "Repeat a rewrite n times." ]
--     , external "all"        (allR :: RewriteH LCore -> RewriteH LCore)
--         [ "Apply a rewrite to all children of the node, requiring success at every child." ] .+ Shallow
--     , external "any"        (anyR :: RewriteH LCore -> RewriteH LCore)
--         [ "Apply a rewrite to all children of the node, requiring success for at least one child." ] .+ Shallow
--     , external "one"        (oneR :: RewriteH LCore -> RewriteH LCore)
--         [ "Apply a rewrite to the first child of the node for which it can succeed." ] .+ Shallow
--     , external "allBU"     (allbuR :: RewriteH LCore -> RewriteH LCore)
--         [ "Promote a rewrite to operate over an entire tree in bottom-up order, requiring success at every node." ] .+ Deep
--     , external "allTD"     (alltdR :: RewriteH LCore -> RewriteH LCore)
--         [ "Promote a rewrite to operate over an entire tree in top-down order, requiring success at every node." ] .+ Deep
--     , external "allDU"     (allduR :: RewriteH LCore -> RewriteH LCore)
--         [ "Apply a rewrite twice, in a top-down and bottom-up way, using one single tree traversal,",
--           "succeeding if they all succeed."] .+ Deep
--     , external "anyBU"     (anybuR :: RewriteH LCore -> RewriteH LCore)
--         [ "Promote a rewrite to operate over an entire tree in bottom-up order, requiring success for at least one node." ] .+ Deep
--     , external "anyTD"     (anytdR :: RewriteH LCore -> RewriteH LCore)
--         [ "Promote a rewrite to operate over an entire tree in top-down order, requiring success for at least one node." ] .+ Deep
--     , external "anyDU"     (anyduR :: RewriteH LCore -> RewriteH LCore)
--         [ "Apply a rewrite twice, in a top-down and bottom-up way, using one single tree traversal,",
--           "succeeding if any succeed."] .+ Deep
--     , external "oneTD"     (onetdR :: RewriteH LCore -> RewriteH LCore)
--         [ "Apply a rewrite to the first node (in a top-down order) for which it can succeed." ] .+ Deep
--     , external "oneBU"     (onebuR :: RewriteH LCore -> RewriteH LCore)
--         [ "Apply a rewrite to the first node (in a bottom-up order) for which it can succeed." ] .+ Deep
--     , external "pruneTD"   (prunetdR :: RewriteH LCore -> RewriteH LCore)
--         [ "Attempt to apply a rewrite in a top-down manner, prunning at successful rewrites." ] .+ Deep
--     , external "innermost"  (innermostR :: RewriteH LCore -> RewriteH LCore)
--         [ "A fixed-point traveral, starting with the innermost term." ] .+ Deep .+ Loop
-- --     , external "focus"      (hfocusR :: TransformH LCore LocalPathH -> RewriteH LCore -> RewriteH LCore)
-- --         [ "Apply a rewrite to a focal point."] .+ Navigation .+ Deep
-- --     , external "focus"      ((\p -> hfocusR (return p)) :: LocalPathH -> RewriteH LCore -> RewriteH LCore)
-- --         [ "Apply a rewrite to a focal point."] .+ Navigation .+ Deep
--     , external "when"       ((>>) :: TransformH LCore () -> RewriteH LCore -> RewriteH LCore)
--         [ "Apply a rewrite only if the check succeeds." ] .+ Predicate
--     , external "forward"    (forwardT :: BiRewriteH LCore -> RewriteH LCore)
--         [ "Apply a bidirectional rewrite forewards." ]
--     , external "backward"   (backwardT :: BiRewriteH LCore -> RewriteH LCore)
--         [ "Apply a bidirectional rewrite backwards." ]
--     , external "anyCall"   (const anyCallR_LCore :: Proxy LCore -> RewriteH LCore -> RewriteH LCore)
--         [ "any-call (.. unfold command ..) applies an unfold command to all applications."
--         , "Preference is given to applications with more arguments." ] .+ Deep
-- --     , external "extract"    (extractR :: RewriteH LCoreTC -> RewriteH LCore)
-- --         [ "Extract a RewriteCore from a RewriteCoreTC" ]
-- --    , external "atPath"     (flip hfocusT idR :: TransformH LCore LocalPathH -> TransformH LCore LCore)
-- --        [ "return the expression found at the given path" ]
-- --    , external "atPath"     (extractT . flip hfocusT projectT :: TransformH LCoreTC LocalPathH -> TransformH LCore LCore)
-- --        [ "return the expression found at the given path" ]

      -- ???
    , external "unfoldRemembered" (promoteExprR . unfoldRememberedR Obligation :: LemmaName -> RewriteH LCore)
        [ "Unfold a remembered definition." ] .+ Deep .+ Context
    ]

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
--     , external "id"         (idR :: RewriteH LCoreTC)
--         [ "Perform an identity rewrite."] .+ Shallow
-- --     , external "focus"      ((\p -> hfocusR (return p)) :: LocalPathH -> RewriteH LCoreTC -> RewriteH LCoreTC)
-- --         [ "Apply a rewrite to a focal point."] .+ Navigation .+ Deep
-- --     , external "focus"      (hfocusR :: TransformH LCoreTC LocalPathH -> RewriteH LCoreTC -> RewriteH LCoreTC)
-- --         [ "Apply a rewrite to a focal point."] .+ Navigation .+ Deep
--     , external ">>>"        ((>>>) :: RewriteH LCoreTC -> RewriteH LCoreTC -> RewriteH LCoreTC)
--         [ "Compose rewrites, requiring both to succeed." ]
--     , external "promote"    (promoteR :: RewriteH LCore -> RewriteH LCoreTC)
--         [ "Promote a RewriteCore to a RewriteCoreTC" ]
--     , external "between"    (betweenR :: Int -> Int -> RewriteH LCoreTC -> RewriteH LCoreTC)
--         [ "between x y rr -> perform rr at least x times and at most y times." ]
-- --    , external "atPath"     (flip hfocusT idR :: TransformH LCoreTC LocalPathH -> TransformH LCoreTC LCoreTC)
-- --        [ "return the expression found at the given path" ]
    ]
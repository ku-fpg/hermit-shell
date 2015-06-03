{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Rewrite where

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

      -- ???
    , external "unfoldRemembered" (promoteExprR . unfoldRememberedR Obligation :: LemmaName -> RewriteH LCore)
        [ "Unfold a remembered definition." ] .+ Deep .+ Context
    , external "anyCall"   (const anyCallR_LCore :: Proxy LCore -> RewriteH LCore -> RewriteH LCore)
        [ "any-call (.. unfold command ..) applies an unfold command to all applications."
        , "Preference is given to applications with more arguments." ] .+ Deep
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
    ]
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Rewrite where

import           Data.Proxy

import           HERMIT.Dictionary
import           HERMIT.Kure
import           HERMIT.Lemma

import           HERMIT.Server.Parser.Name ()
import           HERMIT.Server.Parser.Utils

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
    , external "bash-debug" (bashDebugR :: RewriteH LCore)
        [ "verbose bash - most useful with set-auto-corelint True" ] .+ Eval .+ Deep .+ Loop

      -- ???
    , external "unfoldRemembered" (promoteExprR . unfoldRememberedR Obligation :: LemmaName -> RewriteH LCore)
        [ "Unfold a remembered definition." ] .+ Deep .+ Context
    , external "anyCall"   (const anyCallR_LCore :: Proxy LCore -> RewriteH LCore -> RewriteH LCore)
        [ "any-call (.. unfold command ..) applies an unfold command to all applications."
        , "Preference is given to applications with more arguments." ] .+ Deep
    ]

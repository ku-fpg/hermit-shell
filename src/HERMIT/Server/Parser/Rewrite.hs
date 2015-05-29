{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Rewrite where

import           HERMIT.Dictionary
-- import           HERMIT.Dictionary.Kure
import           HERMIT.Kure
import           HERMIT.Lemma

import           HERMIT.Server.Parser.Name()
import           HERMIT.Server.Parser.Utils

instance External (RewriteH LCore) where
  parseExternals =
    [ external "unfoldRemembered" (promoteExprR . unfoldRememberedR Obligation :: LemmaName -> RewriteH LCore)
        [ "Unfold a remembered definition." ] .+ Deep .+ Context
--     , external "anyCall"   (anyCallR_LCore :: RewriteH LCore -> RewriteH LCore)
--        [ "any-call (.. unfold command ..) applies an unfold command to all applications."
--        , "Preference is given to applications with more arguments." ] .+ Deep
    ]

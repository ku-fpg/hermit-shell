{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Rewrite where

import           Data.Proxy

import           HERMIT.Dictionary
import           HERMIT.Dictionary.Kure
import           HERMIT.Kure
import           HERMIT.Lemma

import           HERMIT.Server.Parser.Name()
import           HERMIT.Server.Parser.Utils
import           HERMIT.Typeable()

instance External (RewriteH LCore) where
  parseExternals =
    [ external "unfoldRemembered" (promoteExprR . unfoldRememberedR Obligation :: LemmaName -> RewriteH LCore)
        [ "Unfold a remembered definition." ] .+ Deep .+ Context
    , external "anyCall"   (const anyCallR_LCore :: Proxy LCore -> RewriteH LCore -> RewriteH LCore)
        [ "any-call (.. unfold command ..) applies an unfold command to all applications."
        , "Preference is given to applications with more arguments." ] .+ Deep
    ]
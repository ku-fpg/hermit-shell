{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Rewrite where

import           HERMIT.Dictionary
import           HERMIT.Kure
import           HERMIT.Lemma

import           HERMIT.Server.Parser.Name()
import           HERMIT.Server.Parser.Utils

instance External (RewriteH LCore) where
  parseExternal = alts 
    [ external "unfoldRemembered" (promoteExprR . unfoldRememberedR Obligation :: LemmaName -> RewriteH LCore)
        [ "Unfold a remembered definition." ] .+ Deep .+ Context
    ]

{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Shell.Proof where

import HERMIT.API.Types

-- | Proof a lemma interactively.
-- proveLemma :: LemmaName -> ShellEffect

assume :: ProofShellCommand
assume = ProofShellCommand $ method "UserAssume" []


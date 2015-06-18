{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Shell.Proof where

import HERMIT.API.Types

import Data.Aeson

-- | Proof a lemma interactively.
proveLemma :: LemmaName -> ShellEffect
proveLemma nm = ShellEffect $ method "proveLemma" [toJSON nm]

-- | mark lemma as assumed
assume :: ProofShellCommand
assume = ProofShellCommand $ method "UserAssume" []


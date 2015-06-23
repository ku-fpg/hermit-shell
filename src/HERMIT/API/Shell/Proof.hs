{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Shell.Proof where

import HERMIT.API.Types

import Data.Aeson

-- | Proof a lemma interactively.
proveLemma :: LemmaName -> ShellEffect
proveLemma nm = ShellEffect $ method "proveLemma" [toJSON nm]

-- | check for alpha-equality, marking the lemma as proven
endProof :: ProofShellCommand
endProof = ProofShellCommand $ method "endProof" []

-- | check for alpha-equality, marking the proof case as proven
endCase :: ProofShellCommand
endCase = ProofShellCommand $ method "endCase" []

-- | mark lemma as assumed
assume :: ProofShellCommand
assume = ProofShellCommand $ method "assume" []


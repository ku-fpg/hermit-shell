{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.Server.Parser.ProofShellCommand where

import HERMIT.Server.Parser.Utils
import HERMIT.Server.Parser.Transform ()

import HERMIT.Shell.Proof

instance External ProofShellCommand where
  parseExternals =
    [ external "UserProof" (PCEnd . UserProof . userProofTechnique)
        [ "Run the technique, mark Proven if succeeds" ]
    , external "assume" (PCEnd UserAssume)
        [ "mark lemma as assumed" ]
    , external "Reflexivity" (PCEnd Reflexivity)
        [ "Check for alpha-equivalence first" ]

    , external "endProof" (PCEnd Reflexivity)
        [ "check for alpha-equality, marking the lemma as proven" ]
    , external "endCase" (PCEnd Reflexivity)
        [ "check for alpha-equality, marking the proof case as proven" ]
    ]


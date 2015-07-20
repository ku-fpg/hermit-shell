{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.Server.Parser.ProofShellCommand where

import HERMIT.Server.Parser.Utils
import HERMIT.Server.Parser.Transform ()

import HERMIT.Shell.Proof

instance External ProofShellCommand where
  parseExternals =
    [ external "UserProof" (PCEnd . UserProof . userProofTechnique)
    , external "assume" (PCEnd UserAssume)
    , external "Reflexivity" (PCEnd Reflexivity)

    , external "endProof" (PCEnd Reflexivity)
    , external "endCase" (PCEnd Reflexivity)
    ]


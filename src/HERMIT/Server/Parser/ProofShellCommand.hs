{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.Server.Parser.ProofShellCommand where

import HERMIT.Server.Parser.Utils
import HERMIT.Server.Parser.Transform ()

import HERMIT.Shell.Proof
import HERMIT.Kure

import Data.Aeson
import Data.Aeson.Types

instance External ProofShellCommand where
  parseExternals =
    [ external "UserProof" (PCEnd . UserProof . userProofTechnique)
        [ "Run the technique, mark Proven if succeeds" ]
    , external "UserAssume" (PCEnd UserAssume)
        [ "Assume" ]
    , external "Reflexivity" (PCEnd Reflexivity)
        [ "Check for alpha-equivalence first" ]
    ]


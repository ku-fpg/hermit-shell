{-# LANGUAGE NoImplicitPrelude #-}
module InductiveStepScript where

import HERMIT.API.Prelude

inductiveStep :: Rewrite LCore
inductiveStep
  = serialise
      [ unfoldWith "++"
      , smash
      ]


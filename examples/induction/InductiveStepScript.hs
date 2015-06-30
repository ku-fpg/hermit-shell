module InductiveStepScript where

import HERMIT.API
import HERMIT.API.Types

inductiveStep :: Rewrite LCore
inductiveStep
  = serialise
      [ unfoldWith "++"
      , smash
      ]


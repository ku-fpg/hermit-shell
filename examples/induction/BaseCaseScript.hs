module BaseCaseScript where

import HERMIT.API
import HERMIT.API.Types

baseCase :: Rewrite LCore
baseCase
  = serialise
      [ oneBU (unfoldWith "++")
      , smash
      ]


{-# LANGUAGE NoImplicitPrelude #-}
module BaseCaseScript where

import HERMIT.API.Prelude

baseCase :: Rewrite LCore
baseCase
  = serialise
      [ oneBU (unfoldWith "++")
      , smash
      ]


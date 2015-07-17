{-# LANGUAGE NoImplicitPrelude #-}
module PreconditionLScript where

import HERMIT.API.Prelude

preconditionL :: Rewrite LCore
preconditionL = do
  unfoldWith "f"
  >>> caseElim


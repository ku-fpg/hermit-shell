{-# LANGUAGE NoImplicitPrelude #-}
module PreconditionRScript where

import HERMIT.API.Prelude

preconditionR :: Rewrite LCore
preconditionR = do
  unfoldWith "h"
  >>> caseElim


{-# LANGUAGE NoImplicitPrelude #-}
module FStrictScript where

import HERMIT.API.Prelude

fstrict :: Rewrite LCore
fstrict = do
  unfoldWith "f"
  >>> undefinedCase


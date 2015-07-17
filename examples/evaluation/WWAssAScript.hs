{-# LANGUAGE NoImplicitPrelude #-}
module WWAssAScript where

import HERMIT.API.Prelude

-- Worker/Wrapper (Result Variant) Assumption A:  abs (rep a) <=> a

wwa :: Rewrite LCore
wwa =
  -- abs (rep a)
  unfoldWith "abs"

  -- rep a Just Nothing
  >>> unfoldWith "rep"

  -- case a of
  --   Nothing -> Nothing
  --   Just n  -> Just n
  >>> caseElimMergeAlts

  -- a


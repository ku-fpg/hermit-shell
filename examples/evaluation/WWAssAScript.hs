{-# LANGUAGE OverloadedStrings #-}
module WWAssAScript where

import HERMIT.API
import HERMIT.API.Types

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


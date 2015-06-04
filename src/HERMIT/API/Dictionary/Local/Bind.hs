{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Local.Bind where

import HERMIT.API.Types

-- | Convert a non-recursive binding into a recursive binding group with a single definition.
-- NonRec v e ==> Rec [Def v e]
nonrecToRec :: Rewrite LCore
nonrecToRec = Transform $ method "nonrecToRec" []

-- | Convert a singleton recursive binding into a non-recursive binding group.
-- Rec [Def v e] ==> NonRec v e,  (v not free in e)
recToNonrec :: Rewrite LCore
recToNonrec = Transform $ method "recToNonrec" []

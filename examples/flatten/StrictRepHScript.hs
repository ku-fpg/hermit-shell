{-# LANGUAGE NoImplicitPrelude #-}
module StrictRepHScript where

import HERMIT.API.Prelude

strictRepH :: Rewrite LCore
strictRepH =
  -- repH ty (undefined [ty])
  unfoldWith "repH"
  -- (++) ty (undefined [ty])
  >>> unfoldRuleUnsafe "++ strict"
  -- undefined ([ty] -> [ty])


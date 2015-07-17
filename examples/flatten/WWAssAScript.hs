{-# LANGUAGE NoImplicitPrelude #-}
module WWAssAScript where

import HERMIT.API.Prelude

wwa :: Rewrite LCore
wwa =
  -- absH (repH x)
  unfoldWith "absH"
  -- repH x []
  >>> unfoldWith "repH"
  -- x ++ []
  >>> unfoldRuleUnsafe "++ []"


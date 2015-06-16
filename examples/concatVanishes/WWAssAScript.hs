{-# LANGUAGE OverloadedStrings #-}
module WWAssAScript where

import HERMIT.API
import HERMIT.API.Dictionary.KURE

wwa :: Rewrite LCore
wwa =
    -- absH (repH x)
  unfoldWith "absH"
    -- repH x []
  >>> unfoldWith "repH"
    -- x ++ []
  >>> unfoldRuleUnsafe "++ []"


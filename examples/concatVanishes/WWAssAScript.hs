{-# LANGUAGE OverloadedStrings #-}
module WWAssAScript where

import HERMIT.API

wwa :: Shell ()
wwa = do
    -- absH (repH x)
  eval "{" ; apply (unfoldWith "absH") ; eval "}"

    -- repH x []
  eval "{" ; apply (unfoldWith "repH") ; eval "}"

    -- x ++ []
  eval "{" ; apply (unfoldRuleUnsafe "++ []") ; eval "}"


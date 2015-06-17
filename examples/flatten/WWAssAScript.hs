module WWAssAScript where

import HERMIT.API
import HERMIT.API.Types

wwa :: Rewrite LCore
wwa = 
  -- absH (repH x)
  unfoldWith "absH"
  -- repH x []
  >>> unfoldWith "repH"
  -- x ++ []
  >>> unfoldRuleUnsafe "++ []"


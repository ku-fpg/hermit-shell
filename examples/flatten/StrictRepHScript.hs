module StrictRepHScript where

import HERMIT.API
import HERMIT.API.Types

strictRepH :: Rewrite LCore
strictRepH =
  -- repH ty (undefined [ty])
  unfoldWith "repH"
  -- (++) ty (undefined [ty])
  >>> unfoldRuleUnsafe "++ strict"
  -- undefined ([ty] -> [ty])


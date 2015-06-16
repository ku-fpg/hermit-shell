module WWAssAScript where

import HERMIT.API
import HERMIT.API.Types

wwa :: Rewrite LCore
wwa =
  -- wrap (unwrap h)
  etaExpand "n"
  -- \ n -> wrap (unwrap h) n
  >>> anyCall (unfoldWith "wrap")
  -- \ n -> fst (unwrap h n)
  >>> anyCall (unfoldWith "unwrap")
  -- \ n -> fst (h n, h (S n))
  >>> bash
  -- h


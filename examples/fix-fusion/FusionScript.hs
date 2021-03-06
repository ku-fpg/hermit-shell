module FusionScript where
import HERMIT.API

import PreconditionLScript
import PreconditionRScript
import FStrictScript

script :: Shell ()
script = do
  setPath $ bindingOf "prog"

  scope $ do setPath (rhsOf "prog")
             apply . forward $ fixFusionRule "f" "g" "h" preconditionL preconditionR fstrict


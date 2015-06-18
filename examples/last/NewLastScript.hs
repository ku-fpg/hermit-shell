import Prelude hiding (repeat)

import HERMIT.API

script :: Shell ()
script = do
  apply flattenModule
  shellEffect $ setPPType Show

  setPath $ bindingOf "last"
  apply $ fixIntro

  scope $ do setPath $ applicationOf "fix"
             apply $ split1Beta "last" "wrap" "unwrap"

               -- prove the assumption
             eval "lhs (repeat (any-call (unfold ['., 'wrap, 'unwrap])))"
             eval "both smash"
             eval "end-proof"

             apply $ repeat (anyCall (unfoldAny ["g", "wrap", "unwrap", "fix"]))
             apply bash


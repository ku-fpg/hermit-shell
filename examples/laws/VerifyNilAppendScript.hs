module VerifyNilAppendScript
  (nilAppend)
  where

import HERMIT.API
import HERMIT.API.Types

nilAppend :: Shell ()
nilAppend = do
  eval "rule-to-lemma \"nil-append\""

  proof "nil-append" $ do
    apply . anyBU $ inlineWith "++"
    apply smash

script :: Shell ()
script = nilAppend


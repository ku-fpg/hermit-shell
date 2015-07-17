{-# LANGUAGE NoImplicitPrelude #-}
module VerifyNilAppendScript
  (nilAppend)
  where

import HERMIT.API.Prelude

nilAppend :: Shell ()
nilAppend = do
  eval "rule-to-lemma nil-append"

  proof "nil-append" $ do
    apply . anyBU $ inlineWith "++"
    apply smash

script :: Shell ()
script = nilAppend


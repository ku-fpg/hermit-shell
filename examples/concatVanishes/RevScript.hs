{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API
import HERMIT.API.Types

import ConcatVanishesScript
import WWAssAScript

wwc :: Rewrite LCore
wwc = wwResultAssAToAssC wwa

doTheWWSplit :: Shell ()
doTheWWSplit = do
  setPath $ bindingOf "rev"
  apply $ wwResultSplitStaticArg 1 [0] "absH" "repH" wwc

script :: Shell ()
script = do
  concatVanishes doTheWWSplit

  -- Assuming unproven lemmas:
  unprovenAssume "++ []"
  unprovenAssume "++ strict"
  unprovenAssume "repH (:)"
  unprovenAssume "repH ++"
  unprovenAssume "repH []"

unprovenAssume :: LemmaName -> Shell ()
unprovenAssume lemmaName = do
  shellEffect $ proveLemma lemmaName
  proofCmd assume


module QSortScript where

import HERMIT.API
import HERMIT.API.Types

import ConcatVanishesScript
import WWAssAScript

wwc :: Rewrite LCore
wwc = wwResultAssAToAssC wwa

doTheWWSplit :: Shell ()
doTheWWSplit = do
  setPath $ bindingOf "qsort"
  apply $ wwResultSplitStaticArg 2 [0] "absH" "repH" wwc

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


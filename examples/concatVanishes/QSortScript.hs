import HERMIT.API

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

unprovenAssume :: String -> Shell ()
unprovenAssume lemmaName = do
  eval $ "prove-lemma " ++ show lemmaName
  proofCmd assume



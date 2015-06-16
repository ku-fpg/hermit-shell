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
script = concatVanishes doTheWWSplit


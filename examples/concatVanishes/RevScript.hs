import HERMIT.API

import ConcatVanishesScript
import WWAssAScript

wwc :: Rewrite LCore
wwc = wwResultAssAToAssC wwa

doTheWWSplit :: Shell ()
doTheWWSplit = do
  setPath $ bindingOf "rev"
  apply $ wwResultSplitStaticArg 1 [0] "absH" "repH" wwc

script :: Shell ()
script = concatVanishes doTheWWSplit


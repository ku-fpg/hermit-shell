{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API
import HERMIT.API.Types

import ConcatVanishesScript

import WWAssAScript

wwc :: Rewrite LCore
wwc = wwResultAssAToAssC wwa

-- XXX: Is there a way to avoid lifting this into Shell ()
--      at this point in the code?
doTheWWSplit :: Shell ()
doTheWWSplit = do
  setPath $ bindingOf "flatten"
  apply $ wwResultSplitStaticArg 1 [0] "absH" "repH" wwc


script :: Shell ()
script = concatVanishes doTheWWSplit


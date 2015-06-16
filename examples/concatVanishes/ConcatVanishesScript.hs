module ConcatVanishesScript where
import HERMIT.API
import HERMIT.API.Types

import WWAssAScript
import StrictRepHScript

concatVanishes :: Shell () -> Shell ()
concatVanishes doTheWWSplit = do
  doTheWWSplit

  apply bash

  scope $ do
    setPath $ rhsOf "work"
    sendCrumb lamBody
    apply $ etaExpand "acc"
    sendCrumb lamBody
    apply $ bashExtendedWith [ push "repH" strictRepH, forward wwResultFusion, unfoldRulesUnsafe ["repH ++", "repH (:)", "repH []"] ]
    apply . try $ bashExtendedWith [pushUnsafe "work"]

  apply . oneTD $ unfoldWith "absH"


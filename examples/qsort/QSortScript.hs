{-# LANGUAGE NoImplicitPrelude #-}
module QSortScript where

import HERMIT.API.Prelude

script :: Shell ()
script = do
  apply flattenModule
  setPath $ bindingOf "qsort"
  apply staticArg
  scope $ do
    setPath $ bindingOf "qsort'"
    apply fixIntro
    sendCrumb defRhs
    apply $ split1Beta "qsort" "absR" "repR" ; proofCmd assume
    setPath $ rhsOf "worker"
    apply $ repeat (anyCall (unfoldAny [".", "fix", "g", "repR", "absR"]))
    apply simplify
    apply $ oneTD (caseFloatArgLemma "repHstrict") ; proofCmd assume
    apply $ innermost letFloat
    apply $ anyTD (unfoldRule "repH ++") ; proofCmd assume
    apply $ anyCall (unfoldRule "repH-absH-fusion") ; proofCmd assume
    apply unshadow
    apply $ anyTD (inlineWith "ds1")
    apply simplify
    apply $ alphaLetWith ["worker"]
    apply $ repeat (anyCall (unfoldRules ["repH (:)", "repH []"]))
    proofCmd assume ; proofCmd assume
  apply $ repeat (anyCall (unfoldAny [".", "absR", "absH"]))
  apply $ innermost letFloat
  apply bash


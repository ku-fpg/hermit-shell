{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API

import WWAssAScript
import StrictRepHScript

wwc :: Rewrite LCore
wwc = wwResultAssAToAssC wwa

script :: Shell ()
script = do
  setPath $ bindingOf "rev"
  apply $ wwResultSplitStaticArg 1 [0] "absH" "repH" wwc
  apply bash
  scope $ do
    setPath $ rhsOf "work"
    apply $ alphaLamWith "ys"
    sendCrumb lamBody
    apply $ etaExpand "acc"
    sendCrumb lamBody
    apply $ bashExtendedWith [ push "repH" strictRepH, forward wwResultFusion, unfoldRulesUnsafe ["repH ++", "repH (:)", "repH []"] ]
  apply . oneTD $ unfoldWith "absH"

  -- Assume unproven lemmas (this is more explicit than having a '-safety=unsafe' flag):
  unprovenAssume "++ []"
  unprovenAssume "++ strict"
  unprovenAssume "repH (:)"
  unprovenAssume "repH ++"
  unprovenAssume "repH []"

unprovenAssume :: String -> Shell ()
unprovenAssume lemmaName = do
  eval $ "prove-lemma " ++ show lemmaName
  proofCmd assume


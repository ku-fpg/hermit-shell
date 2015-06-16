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


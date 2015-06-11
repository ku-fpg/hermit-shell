{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API

script :: Shell ()
script = do
  eval "load-as-rewrite \"WWA\" \"WW-Ass-A.hss\""
  apply flattenModule
  setPath $ bindingOf "eval"
  eval "ww-result-split [| abs |] [| rep |] (ww-result-AssA-to-AssC WWA)"
  scope $ do
    sendCrumb defRhs
    apply  letSubst

    scope $ do
      sendCrumb letBody
      apply $     alphaLam (Just "e")
      sendCrumb   lamBody
      apply $     unfoldWith "abs"

    scope $ do
      setPath $ rhsOf "work"
      apply $     alphaLam (Just "e") ; sendCrumb lamBody
      apply $     unfoldWith "rep"
      apply $     bash
      sendCrumb lamBody; sendCrumb lamBody -- XXX: Is this right?
      scope $ do
        eval "consider case" ; sendCrumb (caseAlt 1) ; sendCrumb altRhs ; apply $ abstract "m"
        eval "consider case" ; sendCrumb (caseAlt 1) ; sendCrumb altRhs ; apply $ abstract "n"
      apply $   anyBU (fold "rep")
      apply $   anyTD (forward wwResultFusion)


{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API

script :: Shell ()
script = do
  eval "load-as-rewrite \"WWA\" \"WW-Ass-A.hss\""
  apply flattenModule
  setPath $ bindingOf "eval"
  eval "ww-result-split [| abs |] [| rep |] (ww-result-AssA-to-AssC WWA)"
  eval "{"; sendCrumb defRhs
  apply  letSubst
  eval "  {"; sendCrumb letBody
  apply $     alphaLam (Just "e")
  sendCrumb   lamBody
  apply $     unfoldWith "abs"
  eval "  }"
  eval "  {"; setPath $ rhsOf "work"
  apply $     alphaLam (Just "e") ; sendCrumb lamBody
  apply $     unfoldWith "rep"
  apply $     bash
  sendCrumb lamBody; sendCrumb lamBody -- XXX: Is this right?
  eval "    {  consider case" ; sendCrumb (caseAlt 1) ; sendCrumb altRhs ; apply $ abstract "m"
  eval "       consider case" ; sendCrumb (caseAlt 1) ; sendCrumb altRhs ; apply $ abstract "n"
  eval "    }"
  apply $   anyBU (fold "rep")
  apply $   anyTD (forward wwResultFusion)
  eval "  }"
  eval "}"


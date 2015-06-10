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
  apply $     alphaLam "e"
  sendCrumb   lamBody
  apply $     unfold ("abs" :: Name)
  eval "  }"
  eval "  {"; setPath $ rhsOf "work"
  apply $     alphaLam "e" ; sendCrumb lamBody
  apply $     unfold ("rep" :: Name)
  apply $     bash
  sendCrumb lamBody; sendCrumb lamBody -- XXX: Is this right?
  eval "    {  consider case" ; sendCrumb (caseAlt 1) ; sendCrumb altRhs ; apply $ abstract "m"
  eval "       consider case" ; sendCrumb (caseAlt 1) ; sendCrumb altRhs ; apply $ abstract "n"
  eval "    }"
  apply $   anyBU (fold "rep")
  apply $   anyTD (forward wwResultFusion)
  eval "  }"
  eval "}"


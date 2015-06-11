{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API

script :: Shell ()
script = do
  eval "load-as-rewrite \"WWA\" \"WW-Ass-A.hss\""
  eval "define-rewrite \"WWC\" \"ww-result-AssA-to-AssC WWA\""
  eval "load-as-rewrite \"StrictRepH\" \"StrictRepH.hss\""
  setPath $ bindingOf "rev"
  eval "ww-result-split-static-arg 1 [0] [| absH |] [| repH |] WWC"
  apply bash
  eval "{"; setPath $ rhsOf "work"
  apply $   alphaLam "ys"
  sendCrumb lamBody
  apply $   etaExpand "acc"
  sendCrumb lamBody
  eval "  bash-extended-with [push 'repH StrictRepH, forward ww-result-fusion, unfold-rules-unsafe [\"repH ++\", \"repH (:)\", \"repH []\"] ]"
  eval "}"
  apply $ oneTD (unfold ("absH" :: Name))


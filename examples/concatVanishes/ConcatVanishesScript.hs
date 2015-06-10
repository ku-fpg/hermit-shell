module ConcatVanishesScript where
import HERMIT.API

concatVanishes :: Shell () -> Shell ()
concatVanishes doTheWWSplit = do
  eval "load-as-rewrite \"WWA\" \"WW-Ass-A.hss\""
  eval "define-rewrite \"WWC\" \"ww-result-AssA-to-AssC WWA\""
  eval "load-as-rewrite \"StrictRepH\" \"StrictRepH.hss\""

--  eval "run-script \"do-the-ww-split\" -- ugly hack because we lack paramaterisable scripts"

  doTheWWSplit

  eval "bash"

  eval "{ rhs-of 'work"
  eval "  lam-body"
  eval "  eta-expand 'acc"
  eval "  lam-body"
  eval "  bash-extended-with [ push 'repH StrictRepH, forward ww-result-fusion, unfold-rules-unsafe [\"repH ++\",\"repH (:)\",\"repH []\"] ]"
  eval "  try (bash-extended-with [push-unsafe 'work])"
  eval "}"
  eval "one-td (unfold 'absH)"


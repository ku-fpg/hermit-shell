import HERMIT.API

script :: Shell ()
script = do
  eval "load-as-rewrite \"WWA\" \"WW-Ass-A.hss\""
  eval "flatten-module"
  eval "binding-of 'fac"
  eval "ww-split [| wrap |] [| unwrap |] (ww-AssA-to-AssC WWA)"
  eval "bash-extended-with [ case-elim-inline-scrutinee , inline [ 'unwrap, 'wrap, '*, '- ] ]"

  eval "{ [def-rhs, let-body] ; alpha-lam 'n } -- cosmetic"

import HERMIT.API
script :: Shell ()
script = do
  eval "load-as-rewrite \"WWB\" \"WW-Ass-B.hss\""
  eval "define-rewrite \"WWC\" \"ww-AssB-to-AssC WWB\""
  eval "flatten-module"
  eval "binding-of 'last"
  eval "ww-split-static-arg 1 [0] [| wrap |] [| unwrap |] WWC"
  eval "bash-extended-with [ inline [ 'f, 'wrap, 'unwrap ] ]"
  eval "unshadow"


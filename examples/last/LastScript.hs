import HERMIT.API

script :: Shell ()
script = do
  eval "load-as-rewrite \"WWB\" \"WW-Ass-B.hss\""
  eval "define-rewrite \"WWC\" \"ww-AssB-to-AssC WWB\""
  apply flattenModule
  setPath $ bindingOf "last"
  eval "ww-split-static-arg 1 [0] [| wrap |] [| unwrap |] WWC"
  apply $ bashExtendedWith [ inlineAny [ "f", "wrap", "unwrap" ] ]
  apply unshadow


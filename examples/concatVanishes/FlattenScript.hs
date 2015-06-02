import HERMIT.API
script :: Shell ()
script = do
  eval "define-script \"do-the-ww-split\" \"binding-of 'flatten ; ww-result-split-static-arg 1 [0] [| absH |] [| repH |] WWC\""
  eval "load-and-run \"ConcatVanishes.hss\""


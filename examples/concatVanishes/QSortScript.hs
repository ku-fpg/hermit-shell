import HERMIT.API
script :: Shell ()
script = do
  eval "define-script \"do-the-ww-split\" \"binding-of 'qsort ; ww-result-split-static-arg 2 [0] [| absH |] [| repH |] WWC\""
  eval "load-and-run \"ConcatVanishes.hss\""


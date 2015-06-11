{-# LANGUAGE OverloadedStrings #-}
import HERMIT.API
import HERMIT.API.Types

import ConcatVanishesScript

import WWAssAScript

wwc :: RewriteH LCore
wwc = wwResultAssAToAssC wwa

doTheWWSplit :: Shell ()
doTheWWSplit = do
  setPath $ bindingOf "flatten"
  apply $ wwResultSplitStaticArg 1 [0] "absH" "repH" wwc
--   eval "ww-result-split-static-arg 1 [0] [| absH |] [| repH |] WWC"


script :: Shell ()
script = do
--  eval "define-script \"do-the-ww-split\" \"binding-of 'flatten ; ww-result-split-static-arg 1 [0] [| absH |] [| repH |] WWC\""
--  eval "load-as-rewrite \"WWA\" \"WW-Ass-A.hss\""
--   eval "define-rewrite \"WWC\" \"ww-result-AssA-to-AssC WWA\""
  eval "load-as-rewrite \"StrictRepH\" \"StrictRepH.hss\""

  -- TODO: Figure out why this isn't working:
--  eval "run-script \"do-the-ww-split\" -- ugly hack because we lack paramaterisable scripts"

  concatVanishes doTheWWSplit


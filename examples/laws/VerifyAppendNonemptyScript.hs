module VerifyAppendNonemptyScript (appendNonempty) where

import HERMIT.API

appendNonempty :: Shell ()
appendNonempty = do
  eval "rule-to-lemma append-nonempty"

  proof "append-nonempty" $ do
    pathS [forallBody,forallBody,forallBody,forallBody] $ do
      pathS [eqRhs] $ do
        apply . oneTD $ unfoldWith "++"
      apply smash

script :: Shell ()
script = appendNonempty


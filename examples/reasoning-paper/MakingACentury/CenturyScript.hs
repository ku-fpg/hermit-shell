module CenturyScript where

import HERMIT.API

script :: Shell ()
script = do
  eval "rule-to-lemma 6.8"
  eval "rule-to-lemma 6.6"
  eval "rule-to-lemma 6.5a"
  eval "rule-to-lemma 6.5b"
  eval "rule-to-lemma map-fusion"

  proof "6.8" $ do
    pathS [forallBody, eqRhs] $ do
      apply . oneTD $ unfoldWith "unzip"
      apply $ lemmaForward "6.6"
      apply . anyBU $ lemmaForward "map-fusion"
      mapM_ apply [oneTD $ lemmaForward "6.5a", oneTD $ lemmaForward "6.5b"]


{-# LANGUAGE NoImplicitPrelude #-}
module MapFusionScript where

import HERMIT.API.Prelude

script :: Shell ()
script = do
  eval "rule-to-lemma map-fusion"

  proof "map-fusion" $ do
    apply $ extensionalityWith "xs"

    apply . anyCall $ unfoldWith "."

    apply $ induction "xs"

      -- undefined and nil cases
    apply $ anyBU (unfoldWith "map" >>> (undefinedCase <+ caseReduce))

    apply simplifyLemma

    pathS [forallBody, consequent] $ do
      apply . oneTD $ lemmaBackward "ind-hyp-0"
      apply simplify


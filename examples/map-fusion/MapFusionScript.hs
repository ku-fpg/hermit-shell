module MapFusionScript where

import HERMIT.API

script :: Shell ()
script = do
--  shellEffect $ setPPType Omit

  eval "rule-to-lemma map-fusion"

  proof "map-fusion" $ do
    apply $ extensionalityWith "xs"
    apply . lhsR $ unfoldWith "."

    apply $ induction "xs"

    pathS [forallBody] $ do
        -- undefined case
      pathS [conjLhs, forallBody] $ do
        apply . rhsR $ unfoldWith "map"
        apply . rhsR $ undefinedCase
        apply . lhsR $ anyBU (unfoldWith "map")
        apply . lhsR $ oneTD bash
        apply . lhsR $ oneTD undefinedCase
        apply reflexivity

        -- nil case
      pathS [conjRhs, conjLhs, forallBody] $ do
        apply . bothR $ anyBU (unfoldWith "map" >>> caseReduce)
        apply reflexivity

        -- cons case
      pathS [conjRhs, conjRhs, forallBody, consequent] $ do
        apply . bothR $ anyBU (unfoldWith "map" >>> caseReduce)

        apply . rhsR . oneTD $ lemmaBackward "ind-hyp-0"

        apply . rhsR . oneTD $ unfoldWith "."
        apply reflexivity

      apply bash -- 'a => true' is true


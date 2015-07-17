{-# LANGUAGE NoImplicitPrelude #-}
module MapFusionScript where

import HERMIT.API.Prelude

script :: Shell ()
script = do
--  shellEffect $ setPPType Omit

  eval "rule-to-lemma map-fusion"

  proof "map-fusion" $ do
    apply $ extensionalityWith "xs"
    apply . lhs $ unfoldWith "."

    apply $ induction "xs"

    pathS [forallBody] $ do
        -- undefined case
      pathS [conjLhs, forallBody] $ do
        apply . rhs $ unfoldWith "map"
        apply . rhs $ undefinedCase
        apply . lhs $ anyBU (unfoldWith "map")
        apply . lhs $ oneTD bash
        apply . lhs $ oneTD undefinedCase
        apply reflexivity

        -- nil case
      pathS [conjRhs, conjLhs, forallBody] $ do
        apply . both $ anyBU (unfoldWith "map" >>> caseReduce)
        apply reflexivity

        -- cons case
      pathS [conjRhs, conjRhs, forallBody, consequent] $ do
        apply . both $ anyBU (unfoldWith "map" >>> caseReduce)

        apply . rhs . oneTD $ lemmaBackward "ind-hyp-0"

        apply . rhs . oneTD $ unfoldWith "."
        apply reflexivity

      apply bash -- 'a => true' is true


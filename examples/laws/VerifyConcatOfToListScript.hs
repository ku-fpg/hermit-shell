module VerifyConcatOfToListScript (concatOfToList) where

import Prelude hiding (replicate)
import HERMIT.API

nilCase :: Rewrite LCore
nilCase
  = serialise
      [ anyBU (inlineWith "map" <+ inlineWith "concat")
      , smash
      ]

consCase :: Rewrite LCore
consCase
  = serialise
      [ anyBU (inlineWith "map" <+ inlineWith "concat")
      , smash
      , anyBU (lemmaForward "ind-hyp-0")
      , replicate 2
          (anyBU (unfoldWith "toList" <+ unfoldWith "++" <+ smash))
      ]

concatOfToList :: Shell ()
concatOfToList = do
  eval "rule-to-lemma \"concat-of-toList\""

  proof "concat-of-toList" $ do
    apply $ induction "xs"

    pathS [forallBody] $ do
        -- undefined case
      pathS [conjLhs] $ do
        apply nilCase

        -- nil case
      pathS [conjRhs, conjLhs] $ do
        apply nilCase

        -- cons case
      pathS [conjRhs, conjRhs, forallBody, consequent] $ do
        apply consCase

script :: Shell ()
script = concatOfToList


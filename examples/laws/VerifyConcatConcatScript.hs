module VerifyConcatConcatScript (concatConcat) where

import VerifyConcatAppendScript

import HERMIT.API

nilLeft :: Rewrite LCore
nilLeft
  = serialise
      [ anyCall (unfoldWith "concat")
      , smash
      ]

nilRight :: Rewrite LCore
nilRight
  = serialise
      [ anyCall (unfoldWith "map")
      , smash
      , oneBU (inlineWith "concat")
      , smash
      ]

consLeft :: Rewrite LCore
consLeft
  = pathR [appArg]
  $ serialise
      [ oneBU (inlineWith "concat")
      , smash
      ]

consRight :: Rewrite LCore
consRight
  = serialise
      [ anyCall (unfoldWith "map")
      , smash
      , oneBU (inlineWith "concat")
      , smash
      , oneBU (lemmaBackward "ind-hyp-0")
      , oneBU (lemmaBackward "concat-append")
      ]

concatConcat :: Shell ()
concatConcat = do
  concatAppend

  eval "rule-to-lemma concat-concat"

  proof "concat-concat" $ do
    apply $ induction "x"

    pathS [forallBody] $ do
        -- undefined case
      pathS [conjLhs] $ do
        apply . bothR $ nilLeft
        apply . oneTD $ unfoldWith "concat"
        apply . oneTD $ unfoldWith "map"
        apply smash

        -- nil case
      pathS [conjRhs, conjLhs] $ do
        apply . lhsR $ nilLeft
        apply . rhsR $ nilRight
        apply reflexivity

        -- cons case
      pathS [conjRhs, conjRhs, forallBody, forallBody, consequent] $ do
        apply . lhsR $ consLeft
        apply . rhsR $ consRight
        apply reflexivity

script :: Shell ()
script = concatConcat


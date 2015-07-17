{-# LANGUAGE NoImplicitPrelude #-}
module VerifyConcatConcatScript (concatConcat) where

import VerifyConcatAppendScript

import HERMIT.API.Prelude

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
        apply . both $ nilLeft
        apply . oneTD $ unfoldWith "concat"
        apply . oneTD $ unfoldWith "map"
        apply smash

        -- nil case
      pathS [conjRhs, conjLhs] $ do
        apply . lhs $ nilLeft
        apply . rhs $ nilRight
        apply reflexivity

        -- cons case
      pathS [conjRhs, conjRhs, forallBody, consequent] $ do
        apply . lhs $ consLeft
        apply . rhs $ consRight
        apply reflexivity

script :: Shell ()
script = concatConcat


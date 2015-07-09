module VerifyConcatAppendScript (concatAppend) where

import HERMIT.API

import VerifyAppendAssocScript

nilLeft :: Rewrite LCore
nilLeft
  = serialise
      [ anyCall (unfoldWith "++")
      , smash
      ]

nilRight :: Rewrite LCore
nilRight
  = serialise
      [ oneBU (unfoldWith "concat")
      , smash
      , anyCall (unfoldWith "++")
      , smash
      ]

consLeft :: Rewrite LCore
consLeft
  = serialise
      [ anyCall (unfoldWith "++")
      , smash
      , anyCall (unfoldWith "concat")
      , smash
      , oneBU (lemmaForward "ind-hyp-0")
      ]

consRight :: Rewrite LCore
consRight
  = serialise
      [ oneBU (unfoldWith "concat")
      , smash
      , oneBU (lemmaBackward "append-assoc")
      ]

concatAppend :: Shell ()
concatAppend = do
  appendAssoc

  eval "rule-to-lemma concat-append"

  proof "concat-append" $ do
    apply $ induction "x"

    pathS [forallBody] $ do
        -- undefined case
      pathS [conjLhs] $ do
        apply nilRight
        apply . oneTD $ unfoldWith "concat"
        apply smash

        -- nil case
      pathS [conjRhs, conjLhs] $ do
        apply $ lhsR nilLeft
        apply $ rhsR nilRight
        apply reflexivity

        -- cons case
      pathS [conjRhs, conjRhs, forallBody, consequent] $ do
        apply $ lhsR consLeft
        apply $ rhsR consRight
        apply reflexivity

script :: Shell ()
script = concatAppend


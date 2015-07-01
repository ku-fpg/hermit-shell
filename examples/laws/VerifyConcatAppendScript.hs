module VerifyConcatAppendScript (concatAppend) where

import HERMIT.API

import VerifyAppendAssocScript

concatAppendNilLeft :: Rewrite LCore
concatAppendNilLeft
  = serialise
      [ anyCall (unfoldWith "++")
      , smash
      ]

concatAppendNilRight :: Rewrite LCore
concatAppendNilRight
  = serialise
      [ oneBU (unfoldWith "concat")
      , smash
      , anyCall (unfoldWith "++")
      , smash
      ]

concatConsLeft :: Rewrite LCore
concatConsLeft
  = serialise
      [ anyCall (unfoldWith "++")
      , smash
      , anyCall (unfoldWith "concat")
      , smash
      , oneBU (lemmaForward "ind-hyp-0")
      ]

concatConsRight :: Rewrite LCore
concatConsRight
  = serialise
      [ oneBU (unfoldWith "concat")
      , smash
      , oneBU (lemmaBackward "append-assoc")
      ]

concatAppend :: Shell ()
concatAppend = do
  appendAssoc

  eval "rule-to-lemma \"concat-append\""

  proof "concat-append" $ do
    apply $ induction "x"

    pathS [forallBody] $ do
        -- undefined case
      pathS [conjLhs] $ do
        apply concatAppendNilRight
        apply . oneTD $ unfoldWith "concat"
        apply smash

        -- nil case
      pathS [conjRhs, conjLhs] $ do
        apply $ lhsR concatAppendNilLeft
        apply $ rhsR concatAppendNilRight
        apply reflexivity

        -- cons case
      pathS [conjRhs, conjRhs, forallBody, consequent] $ do
        apply $ lhsR concatConsLeft
        apply $ rhsR concatConsRight
        apply reflexivity

script :: Shell ()
script = concatAppend


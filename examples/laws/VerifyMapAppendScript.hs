module VerifyMapAppendScript (mapAppend) where

import VerifyAppendNonemptyScript
import VerifyMapNonemptyScript

import HERMIT.API

nilLeft :: Rewrite LCore
nilLeft
  = serialise
      [ anyCall (unfoldWith "++")
      , smash
      ]

nilRight :: Rewrite LCore
nilRight
  = serialise
      [ pathR [appFun, appArg]
          $ serialise
              [ unfoldWith "map"
              , smash
              ]
      , unfoldWith "++"
      , smash
      ]

consLeft :: Rewrite LCore
consLeft
  = serialise
      [ oneBU (inlineWith "++")
      , smash
      , oneBU (inlineWith "map")
      , smash
      , oneBU (lemmaForward "ind-hyp-0")
      , lemmaForward "append-nonempty"
      , oneBU (lemmaBackward "map-nonempty")
      ]

mapAppend :: Shell ()
mapAppend = do
  eval "rule-to-lemma \"map-append\""

  appendNonempty
  mapNonempty

  proof "map-append" $ do
    apply $ induction "x"

    pathS [forallBody] $ do
        -- undefined case
      pathS [conjLhs] $ do
        apply . anyBU $ inlineWith "map"
        apply . anyBU $ inlineWith "++"
        apply smash

        -- nil case
      pathS [conjRhs, conjLhs] $ do
        apply . lhsR $ nilLeft
        apply . rhsR $ nilRight
        apply reflexivity

        -- cons case
      pathS [conjRhs, conjRhs, forallBody, consequent] $ do
        apply . lhsR $ consLeft
        apply reflexivity

script :: Shell ()
script = mapAppend


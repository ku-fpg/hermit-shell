module VerifyMapConcatScript (mapConcat) where

import VerifyMapAppendScript
import VerifyConcatNonemptyScript

import HERMIT.API

nilLeft :: Rewrite LCore
nilLeft
  = serialise
      [ anyCall (unfoldAny ["concat", "map"])
      , smash
      ]

nilRight :: Rewrite LCore
nilRight
  = serialise
      [ pathR [appArg]
          $ serialise
              [ oneBU (unfoldWith "map")
              , smash
              ]
      , unfoldWith "concat"
      , smash
      ]

consLeft :: Rewrite LCore
consLeft
  = serialise
      [ anyCall (unfoldWith "concat")
      , smash
      , lemmaForward "map-append"
      , oneBU (lemmaForward "ind-hyp-0")
      ]

consRight :: Rewrite LCore
consRight
  = serialise
      [ pathR [appArg]
          $ serialise
              [ unfoldWith "map"
              , smash
              ]
      , lemmaForward "concat-nonempty"
      ]

mapConcat :: Shell ()
mapConcat = do
  eval "rule-to-lemma \"map-concat\""

  mapAppend
  concatNonempty

  proof "map-concat" $ do
    apply $ induction "xs"

    pathS [forallBody] $ do
        -- undefined case
      pathS [conjLhs] $ do
        apply . anyBU $ unfoldWith "map"
        apply . anyBU $ unfoldWith "concat"
        apply smash

        -- nil case
      pathS [conjRhs, conjLhs] $ do
        apply . lhsR $ nilLeft
        apply . rhsR $ nilRight
        apply reflexivity

        -- cons case
      pathS [conjRhs, conjRhs, forallBody, consequent] $ do
        apply . lhsR $ consLeft
        apply . rhsR $ consRight
        apply reflexivity

script :: Shell ()
script = mapConcat


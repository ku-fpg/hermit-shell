module VerifyMapComposeScript (mapCompose) where

import HERMIT.API

composeLeft :: Rewrite LCore
composeLeft
  = serialise
      [ oneBU (inlineWith "map")
      , smash
      ]

nilRight :: Rewrite LCore
nilRight
  = serialise
      [ pathR [appArg]
          $ serialise
              [ oneBU (inlineWith "map")
              , smash
              ]
      , oneBU (inlineWith "map")
      , smash
      ]

consRight :: Rewrite LCore
consRight
  = serialise
      [ pathR [appArg]
          $ serialise
              [ oneBU (inlineWith "map")
              , smash
              ]
      , oneBU (inlineWith "map")
      , smash
      , oneBU (lemmaBackward "ind-hyp-0")
      , oneBU (inlineWith ".")
      , smash
      ]

mapCompose :: Shell ()
mapCompose = do
  eval "rule-to-lemma map-compose"

  proof "map-compose" $ do
    apply $ induction "xs"

    pathS [forallBody, forallBody, forallBody, forallBody, forallBody] $ do
        -- undefined case
      pathS [conjLhs] $ do
        apply . anyBU $ inlineWith "map"
        apply smash

        -- nil case
      pathS [conjRhs, conjLhs] $ do
        apply . rhsR $ nilRight
        apply . lhsR $ unfoldWith "map"
        apply smash

        -- cons case
      pathS [conjRhs, conjRhs, forallBody, forallBody, consequent] $ do
        apply . rhsR $ consRight
        apply . oneBU $ inlineWith "map"
        apply smash
        pathS [eqLhs] $ do
          apply . oneBU $ inlineWith "."
          apply smash
        apply reflexivity

script :: Shell ()
script = mapCompose


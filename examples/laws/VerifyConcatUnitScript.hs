module VerifyConcatUnitScript (concatUnit) where

import VerifyAppendNilScript

import HERMIT.API

concatUnit :: Shell ()
concatUnit = do
  eval "rule-to-lemma \"concat-unit\""

  appendNil

  proof "concat-unit" $ do
    pathS [forallBody] $ do
      mapM_ apply
            [ oneBU (inlineWith "concat"), smash
              , oneBU (inlineWith "concat"), smash
              , oneBU (lemmaForward "append-nil")
            ]

script :: Shell ()
script = concatUnit


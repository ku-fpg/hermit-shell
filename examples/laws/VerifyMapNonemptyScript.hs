module VerifyMapNonemptyScript (mapNonempty) where

import HERMIT.API

mapNonempty :: Shell ()
mapNonempty = do
  eval "rule-to-lemma map-nonempty"

  proof "map-nonempty" $ do
    pathS [forallBody,forallBody,forallBody,forallBody] $ do
      apply . lhsR $ unfoldWith "map"
      apply smash

script :: Shell ()
script = mapNonempty


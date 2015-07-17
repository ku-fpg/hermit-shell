{-# LANGUAGE NoImplicitPrelude #-}
module VerifyMapNonemptyScript (mapNonempty) where

import HERMIT.API.Prelude

mapNonempty :: Shell ()
mapNonempty = do
  eval "rule-to-lemma map-nonempty"

  proof "map-nonempty" $ do
    pathS [forallBody] $ do
      apply . lhsR $ unfoldWith "map"
      apply smash

script :: Shell ()
script = mapNonempty


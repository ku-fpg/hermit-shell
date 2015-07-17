{-# LANGUAGE NoImplicitPrelude #-}
module VerifyConcatNonemptyScript (concatNonempty) where

import HERMIT.API.Prelude

oneSide :: Rewrite LCore
oneSide
  = serialise
      [ unfoldWith "concat"
      , smash
      ]

concatNonempty :: Shell ()
concatNonempty = do
  eval "rule-to-lemma concat-nonempty"

  proof "concat-nonempty" $ do
    pathS [forallBody] $ do
      apply . both $ oneSide

script :: Shell ()
script = concatNonempty


{-# LANGUAGE NoImplicitPrelude #-}
module VerifyListLeftUnitScript (listLeftUnit) where

import VerifyConcatUnitScript

import HERMIT.API.Prelude

import Control.Monad (replicateM_)

script :: Shell ()
script = listLeftUnit

listLeftUnit :: Shell ()
listLeftUnit = do
  eval "rule-to-lemma left-unit"

  concatUnit

  proof "left-unit" $ do
    apply . anyBU $ inlineWith ["bind", "retur"]
    apply smash
    apply . anyBU $ inlineWith "toList"
    apply smash

    replicateM_ 2 $ do
      apply . anyBU $ inlineWith "map"
      apply smash

    pathS [forallBody] $ do
      apply . lhs $ lemmaForward "concat-unit"
      apply reflexivity


{-# LANGUAGE OverloadedStrings #-}
module ListScript where

import Prelude hiding (repeat)

import HERMIT.API

script :: Shell ()
script = do
  eval "rule-to-lemma return-left"
  query $ copyLemma "return-left" "return-left-list"

  query $ instLemma "return-left-list" "m" "[]"

  proof "return-left-list" $ do
    apply instDictionaries

    stopScript
    scope $ do
      setPath $ applicationOf "return"
      apply $ repeat (unfold <+ smash)



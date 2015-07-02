{-# LANGUAGE OverloadedStrings #-}
module IVerifyMapAppendScript (imapAppend) where

import HERMIT.API

script :: Shell ()
script = imapAppend

imapAppend :: Shell ()
imapAppend = do
  eval "rule-to-lemma map-append"

  shellEffect $ setPPType Show

  proof "map-append" $ do
    apply $ induction "x"
    pathS [forallBody] $ do
        -- undefined case
      pathS [conjLhs] $ do
        apply $ bothR unfold
        apply . bothR $ oneTD unfold
        apply $ bothR smash
        apply reflexivity

        -- nil case
      pathS [conjRhs, conjLhs] $ do
        apply $ bothR unfold
        apply . oneTD $ unfold
        apply $ bothR smash
        pathS [forallBody, eqRhs] $ do
          apply $ oneBU unfold
          apply smash
          apply unfold
        apply reflexivity

        -- cons case
      pathS [conjRhs, conjRhs, forallBody, consequent] $ do
        apply $ bothR unfold
        apply . bothR $ oneTD unfold
        apply $ bothR smash
        apply . oneTD $ lemmaForward "ind-hyp-0"
        apply reflexivity
      apply smash -- 'a => true' is true

  eval "show-lemmas"


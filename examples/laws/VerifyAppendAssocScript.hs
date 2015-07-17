{-# LANGUAGE NoImplicitPrelude #-}
module VerifyAppendAssocScript
  (appendAssoc)
  where

import HERMIT.API.Prelude

import VerifyNilAppendScript

appendAssoc :: Shell ()
appendAssoc = do
  eval "rule-to-lemma append-assoc"

  nilAppend

  proof "append-assoc" $ do
    apply $ induction "x"

    pathS [forallBody] $ do
      -- undefined case
      pathS [conjLhs] $ do
        pathS [forallBody] $ do
          apply . both . oneBU $ inlineWith "++"
          apply smash
          apply . pathR [eqRhs] . oneTD $ inlineWith "++"
          apply smash

      -- nil case
      pathS [conjRhs, conjLhs] $ do
        pathS [forallBody] $ do
          apply . pathR [eqLhs] . oneBU $ inlineWith "++"
          apply smash
          apply . pathR [eqRhs, appFun, appArg] $ lemmaForward "nil-append"
          apply reflexivity

      -- cons case
      pathS [conjRhs, conjRhs, forallBody, consequent] $ do
        apply . oneTD $ inlineWith "++"
        apply smash
        apply . oneTD $ lemmaForward "ind-hyp-0"

          -- Float (:) application out
        pathS [eqRhs] $ do
          apply . oneBU $ inlineWith "++"
          apply smash
          apply . oneBU $ inlineWith "++"
          apply smash
        apply reflexivity
    apply smash  -- 'a => true' is true

script :: Shell ()
script = appendAssoc


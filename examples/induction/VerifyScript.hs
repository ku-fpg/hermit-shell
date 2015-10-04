module VerifyScript where

import HERMIT.API

import BaseCaseScript
import InductiveStepScript

script :: Shell ()
script =
  scope $ do
    setPath progEnd
    eval "rule-to-lemma \"++ []\""

    proof "++ []" $ do
      apply $ induction "xs"
      pathS [forallBody] $ do
           -- undefined case
        apply $ pathR [conjLhs] baseCase

          -- nil case
        apply $ pathR [conjRhs, conjLhs] baseCase

          -- cons case
        pathS [conjRhs, conjRhs, forallBody, forallBody, consequent] $ do
          pathS [eqLhs] $ do
            apply inductiveStep
            apply $ pathR [appArg] $ lemmaForward "ind-hyp-0"
          apply reflexivity


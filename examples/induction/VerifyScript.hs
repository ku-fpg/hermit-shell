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
      apply
        . pathS [forallBody]
        $ serialise
            [   -- undefined case
              pathS [conjLhs] baseCase

                -- nil case
            , pathS [conjRhs, conjLhs] baseCase

                -- cons case
            , pathS [conjRhs, conjRhs, forallBody, consequent]
                $ serialise 
                    [ pathS [eqLhs]
                        $ serialise
                            [ inductiveStep
                            , pathS [appArg]
                                $ lemmaForward "ind-hyp-0"
                            ]
                    , reflexivity
                    ]
            ]


module VerifyAppendNilScript (appendNil) where

  --------------------------------------------
  -- Verify append-nil
  --
  -- forall xs.  xs ++ [] = xs
  --
  --------------------------------------------

import HERMIT.API
import HERMIT.API.Types

appendNilNil :: Rewrite LCore
appendNilNil
  = serialise
    [ oneBU (inlineWith "++")
    , smash
    ]

appendNilUndefined :: Rewrite LCore
appendNilUndefined = appendNilNil

appendNilCons :: Rewrite LCore
appendNilCons
  = serialise
    [ oneBU (inlineWith "++")
    , smash
    , oneBU (lemmaForward "ind-hyp-0")
    ]

appendNil :: Shell ()
appendNil = do
  eval "rule-to-lemma append-nil"

  proof "append-nil" $ do
    apply $ induction "xs"

    pathS [forallBody] $ do
      apply $ pathR [conjLhs]                                  appendNilUndefined
      apply $ pathR [conjRhs, conjLhs]                         appendNilNil

      apply $ pathR [conjRhs, conjRhs, forallBody, consequent] appendNilCons

script :: Shell ()
script = appendNil

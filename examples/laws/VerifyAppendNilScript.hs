module VerifyAppendNilScript where

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

script :: Shell ()
script = do
  --------------------------------------------
  -- Verify append-nil
  --
  -- forall xs.  xs ++ [] = xs
  --
  --------------------------------------------

  -- TODO: Figure out why `loadAndRun` isn't working and update comment.
  -- To test this script:
  --   prog-end
  --   load-and-run "verify-append-nil.hec"
  --   show-lemmas

  eval "rule-to-lemma \"append-nil\""

  proof "append-nil" $ do
    apply $ induction "xs"

    pathS [forallBody] $ do
      apply $ pathR [conjLhs]                                  appendNilUndefined
      apply $ pathR [conjRhs, conjLhs]                         appendNilNil

        -- XXX: Is it ok that we don't need to do anything with the antecedent here?
      apply $ pathR [conjRhs, conjRhs, forallBody, consequent] appendNilCons


module VerifyAppendAssocScript
  (appendAssoc)
  where

import HERMIT.API

import VerifyNilAppendScript

appendAssoc :: Shell ()
appendAssoc = do
  eval "rule-to-lemma \"append-assoc\""

script :: Shell ()
script = appendAssoc


module VerifyListMonoidAssocScript (listMonoidAssoc) where

import VerifyAppendAssocScript

import HERMIT.API

assocLeft :: Rewrite LCore
assocLeft
  = serialise
      [ anyBU (inlineWith "mappen")
      , smash
      , anyBU (lemmaForward "append-assoc")
      ]

assocRight :: Rewrite LCore
assocRight
  = serialise
      [ anyBU (inlineWith "mappen")
      , smash
      ]

listMonoidAssoc :: Shell ()
listMonoidAssoc = do
  eval "rule-to-lemma monoid-assoc"

  appendAssoc

  proof "monoid-assoc" $ do
    apply . lhsR $ assocLeft
    apply . rhsR $ assocRight

script :: Shell ()
script = listMonoidAssoc


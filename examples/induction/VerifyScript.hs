module VerifyScript where
import HERMIT.API

script :: Shell ()
script = do
  --load "BaseCase" "BaseCase.her"
  --load "InductiveStep" "InductiveStep.her"
  eval "load-as-rewrite \"BaseCase\" \"BaseCase.her\""
  eval "load-as-rewrite \"InductiveStep\" \"InductiveStep.her\""

  eval "{ prog-end"
  eval "  rule-to-lemma \"++ []\""
  --  verify-lemma "++ []" (inductive-proof 'xs [ '"[]" , ': ] [ BaseCase , InductiveStep ])
  eval "  prove-lemma \"++ []\""
  eval "  induction 'xs"
  eval "  forall-body"
  eval "     -- undefined case"
  eval "  { conj-lhs"
  eval "    BaseCase"
  eval "  }"

  eval "    -- nil case"
  eval "  { [conj-rhs, conj-lhs]"
  eval "    BaseCase"
  eval "  }"

  eval "    -- cons case"
  eval "  { [conj-rhs, conj-rhs, forall-body, consequent]"
  eval "    { eq-lhs"
  eval "      InductiveStep"
  eval "      { [app-arg]"
  eval "        lemma-forward ind-hyp-0"
  eval "      }"
  eval "    }"
  eval "    reflexivity"
  eval "  }"
  eval "  end-proof"
  eval "}"



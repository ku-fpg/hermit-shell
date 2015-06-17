import HERMIT.API
script :: Shell ()
script = do
  eval "set-pp-type Show"

  eval "flatten-module"

  eval "binding-of 'nub"
  eval "fix-intro ; def-rhs"
  eval "split-2-beta nub [| absN |] [| repN |]" ; proofCmd assume

  -- this bit to essentially undo the fix-intro
  eval "{ application-of 'repN ; app-arg ; let-intro 'nub ; one-td (unfold 'fix) ; simplify }"
  eval "innermost let-float"
  eval "alpha-let ['nub'] -- rename x to nub'"

  -- back to the derivation
  eval "binding-of 'worker"
  eval "one-td (unfold 'repN)"
  eval "remember origworker"
  eval "one-td (unfold 'filter)"
  eval "one-td (case-float-arg-lemma nubStrict)"

  -- prove strictness condition
  eval "lhs unfold ; smash ; end-proof"

  eval "one-td (unfold 'nub')"
  eval "simplify"

  eval "one-td (case-float-arg-lemma nubStrict)"

  -- prove strictness condition
  eval "lhs unfold ; smash ; end-proof"

  eval "{ consider case ; consider case ; case-alt 1 ; alt-rhs"
  eval "  unfold ; simplify"
  eval "  one-td (unfold-rule \"filter-fusion\")" ; proofCmd assume
  eval "  simplify"
  eval "  one-td (unfold-rule \"member-fusion\")" ; proofCmd assume
  eval "}"
  eval "nonrec-to-rec"
  eval "any-td (fold-remembered origworker)"


module HanoiScript where
import HERMIT.API

script :: Shell ()
script = do
  eval "flatten-module"

  -- do the w/w split
  eval "binding-of 'hanoi"
  eval "{ ww-split-unsafe [| wrap |] [| unwrap |] }"

  eval "{ binding-of 'work"
  eval "  remember origwork"

  eval "  any-call (unfold 'unwrap)"

  eval "  -- establish the zero base case"
  eval "  [ def-rhs, lam-body, lam-body, lam-body, lam-body]"
  eval "  case-split-inline 'n"
  eval "  { case-alt 0 ; any-call (unfold 'f) ; simplify }"

  eval "  -- establish the one base case"
  eval "  { [case-alt 1, alt-rhs] ; case-split-inline 'a"
  eval "    { case-alt 0 ; any-call (unfold 'f) ; simplify"
  eval "      any-call (unfold-remembered origwork)"
  eval "      any-call (forward (ww-assumption-A-unsafe [| wrap |] [| unwrap |]))"
  eval "      any-call (unfold 'f)"
  eval "      simplify"
  eval "      any-call (unfold-rule \"[] ++\")"
  proofCmd assume
  --      any-call (unfold-rule "++ []")
  eval "    }"
  eval "    { case-alt 1 ; any-call (unfold 'f) ; simplify"

  eval "      any-call (unfold-remembered origwork)"
  eval "      any-call (forward (ww-assumption-A-unsafe [| wrap |] [| unwrap |]))"
  eval "      any-call (unfold 'f)"
  eval "      innermost let-subst ; simplify"

  eval "      -- recursion decrements by two, so must do this again"
  eval "      any-call (unfold-remembered origwork)"
  eval "      any-call (forward (ww-assumption-A-unsafe [| wrap |] [| unwrap |]))"

  eval "      -- time to let intro"
  eval "      -- need a \"occurance 'work\" like consider"
  eval "      { alt-rhs"
  eval "        { arg 5"
  eval "          { arg 1"
  eval "            { arg 1 ; let-intro 'u }"
  eval "            { arg 2 ; arg 2 ; let-intro 'v }"
  eval "          }"
  eval "          { arg 2 ; arg 2 ; arg 1 ; let-intro 'w }"
  eval "        }"
  eval "        innermost let-float"
  eval "        try (reorder-lets ['u,'v,'w])"
  eval "        any-call (fold 'u)"
  eval "        any-call (fold 'v)"
  --        any-call (fold 'w)
  eval "        let-tuple 'uvw"
  eval "        any-call (fold 'unwrap)"
  eval "        any-call (fold-remembered origwork)"
  eval "      }"
  eval "    }"
  eval "  }"
  eval "}"
  --innermost let-elim
  eval "innermost let-subst"


-- NOTE: Not working
import HERMIT.API
script :: Shell ()
script = do
  eval "flatten-module"
  eval "set-pp-type Show"

  eval "binding-of 'last"
  eval "fix-intro"
  eval "{ application-of 'fix"
  eval "  split-1-beta last [| wrap |] [| unwrap |]"
  eval "  -- prove the assumption"
  eval "  lhs (repeat (any-call (unfold ['., 'wrap, 'unwrap])))"
  eval "  both smash"
  eval "  end-proof"

  eval "  repeat (any-call (unfold ['g, 'wrap, 'unwrap, 'fix]))"
  eval "  bash"
  eval "}"


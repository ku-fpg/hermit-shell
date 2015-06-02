import HERMIT.API
script :: Shell ()
script = do
  eval "load-as-rewrite \"WWA\" \"WW-Ass-A.hss\""
  eval "flatten-module"

  eval "binding-of 'fib"

  eval "{"

  eval "ww-split [| wrap |] [| unwrap |] (ww-AssA-to-AssC WWA)"
  eval "binding-of 'work ; remember origwork"

  eval "-- work = unwrap (f (wrap work))"

  eval "def-rhs ; eta-expand 'n"

  eval "-- work n = unwrap (f (wrap work)) n"

  eval "any-call (unfold 'unwrap)"

  eval "-- work n = (f (wrap work) n, f (wrap work) (n+1))"

  eval "lam-body ; case-split-inline 'n"

  eval "-- work 0     = (f (wrap work) 0, f (wrap work) 1)"
  eval "-- work (n+1) = (f (wrap work) (n+1), f (wrap work) (n+2))"

  eval "{ case-alt 0 ; any-call (unfold 'f) }"
  eval "{ [ case-alt 1, alt-rhs, app-arg] ; any-call (unfold 'f) }"
  eval "simplify"

  eval "-- work 0     = (0, 1)"
  eval "-- work (n+1) = (f (wrap work) (n+1), wrap work (n+1) + wrap work n)"

  eval "[ case-alt 1, alt-rhs ]"
  eval "{ app-arg ; any-call (unfold-remembered origwork) }"

  eval "-- work 0     = (0, 1)"
  eval "-- work (n+1) = (f (wrap work) (n+1), wrap (unwrap (f (wrap work))) (n+1) + wrap (unwrap (f (wrap work))) n)"

  eval "any-bu (forward (ww-assumption-A [| wrap |] [| unwrap |] WWA ))"

  eval "-- work 0     = (0, 1)"
  eval "-- work (n+1) = (f (wrap work) (n+1), f (wrap work) (n+1) + f (wrap work) n)"

  eval "{ arg 3 ; arg 1 ; let-intro 'x }"
  eval "{ arg 2 ; let-intro 'y }"
  eval "innermost let-float"
  eval "try (reorder-lets ['x,'y])"
  eval "one-td (fold 'y)"
  eval "let-tuple 'xy"

  eval "-- work 0     = (0, 1)"
  eval "-- work (n+1) = let (x,y) = (f (wrap work) n, f (wrap work) (n+1)) in (y,x+y)"

  eval "one-td (fold 'unwrap)"

  eval "-- work 0     = (0, 1)"
  eval "-- work (n+1) = let (x,y) = unwrap (f (wrap work)) n in (y,x+y)"

  eval "one-td (fold-remembered origwork)"

  eval "-- work 0     = (0, 1)"
  eval "-- work (n+1) = let (x,y) = work n in (y,x+y)"

  eval "}"

  eval "{ def-rhs ; let-elim }"

  eval "any-call (unfold 'wrap)"


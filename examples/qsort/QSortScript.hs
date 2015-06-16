import HERMIT.API
script :: Shell ()
script = do
  eval "flatten-module"
  eval "binding-of 'qsort"
  eval "static-arg"
  eval "{"
  eval "    binding-of 'qsort'"
  eval "    fix-intro"
  eval "    def-rhs"
  eval "    split-1-beta \"qsort\" [|absR|] [|repR|] ; prove-lemma \"qsort-fusion\"" ; proofCmd assume -- XXX: This works...
  eval "    rhs-of 'worker"
  eval "    repeat (any-call (unfold ['.,'fix,'g,'repR,'absR]))"
  eval "    simplify"
  eval "    one-td (case-float-arg-lemma repHstrict)" ; -- proofCmd assume
  eval "    innermost let-float"
  eval "    any-td (unfold-rule \"repH ++\")" ; -- proofCmd assume
  eval "    any-call (unfold-rule repH-absH-fusion)" ; -- proofCmd assume
  eval "    unshadow"
  eval "    any-td (inline 'ds1)"
  eval "    simplify"
  eval "    alpha-let [worker]"
  eval "    repeat (any-call (unfold-rules [\"repH (:)\",\"repH []\"]))"
--  proofCmd assume ; --proofCmd assume
  eval "}"
  eval "repeat (any-call (unfold ['.,'absR, 'absH]))"
  eval "innermost let-float"
  eval "bash"


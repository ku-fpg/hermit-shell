import HERMIT.API
script :: Shell ()
script = do
  eval "flatten-module"
  eval "rule-to-lemma \"++ []\""

  -- module main:Main where
  --   absR :: forall a . ([a] -> H a) -> [a] -> [a]
  --   repR :: forall a . ([a] -> [a]) -> [a] -> H a
  --   rev :: forall a . [a] -> [a]
  --   main :: IO ()
  --   main :: IO ()

  eval "prove-lemma \"++ []\""

  -- Goal:
  -- forall * xs. (++) * xs ([] *) = xs

  eval "lhs (one-td (unfold-rule appendFix))"

  -- Goal:
  -- forall *. (++) * = myAppend *

  eval "assume -- proven appendFix"

  -- Goal:
  -- forall * xs. myAppend * xs ([] *) = xs

  eval "induction 'xs"

  -- Goal:
  -- forall *. myAppend * (undefined *) ([] *) = undefined *

  eval "any-bu (unfold 'myAppend >>> undefined-expr)"
  eval "any-bu (unfold 'myAppend >>> case-reduce)"
  eval "simplify-lemma"
  eval "forall-body ; consequent"
  eval "one-td (lemma-forward ind-hyp-0)"
  eval "end-case"

  eval "rule-to-lemma \"myAppend-assoc\""
  eval "prove-lemma \"myAppend-assoc\""
  eval "induction 'xs"
  eval "{ forall-body"
  eval "    { conj-lhs"
  eval "      any-bu ((unfold 'myAppend) >>> undefined-case)"
  eval "      reflexivity"
  eval "    }"
  eval "    conj-rhs"
  eval "    { conj-lhs"
  eval "      any-bu ((unfold 'myAppend) >>> case-reduce)"
  eval "      reflexivity"
  eval "    }"
  eval "    { conj-rhs"
  eval "      forall-body ; consequent"
  eval "      any-bu (unfold 'myAppend)"
  eval "      smash"
  eval "      rhs (one-td (fold 'myAppend))"
  eval "      one-td (lemma-forward ind-hyp-0)"
  eval "      reflexivity"
  eval "    }"
  eval "}"
  eval "end-proof"

  eval "rule-to-lemma \"repH []\""

  -- module main:Main where
  --   absR :: forall a . ([a] -> H a) -> [a] -> [a]
  --   repR :: forall a . ([a] -> [a]) -> [a] -> H a
  --   rev :: forall a . [a] -> [a]
  --   main :: IO ()
  --   main :: IO ()

  eval "prove-lemma \"repH []\""

  -- Goal:
  -- forall *. repH * ([] *) = id *

  eval "lhs unfold"

  -- Goal:
  -- forall *. (++) * ([] *) = id *

  eval "extensionality"

  -- Goal:
  -- forall * x. (++) * ([] *) x = id * x

  eval "lhs (one-td (unfold-rule appendFix))"

  -- Goal:
  -- forall * x. myAppend * ([] *) x = id * x

  eval "lhs unfold"

  -- Goal:
  -- forall * x.
  -- case [] * of wild *
  --   [] -> x
  --   (:) x xs -> (:) * x (myAppend * xs x)
  -- =
  -- id * x

  eval "both smash"

  -- Goal:
  -- forall * x. x = x

  eval "end-proof -- proven \"repH []\""
  eval "rule-to-lemma \"repH (:)\""

  -- module main:Main where
  --   absR :: forall a . ([a] -> H a) -> [a] -> [a]
  --   repR :: forall a . ([a] -> [a]) -> [a] -> H a
  --   rev :: forall a . [a] -> [a]
  --   main :: IO ()
  --   main :: IO ()

  eval "prove-lemma \"repH (:)\""

  -- Goal:
  -- forall * x xs. repH * ((:) * x xs) = (.) * * * ((:) * x) (repH * xs)

  eval "both (any-call (unfold 'repH))"

  -- Goal:
  -- forall * x xs. (++) * ((:) * x xs) = (.) * * * ((:) * x) ((++) * xs)

  eval "both (any-call (unfold-rule appendFix))"

  -- Goal:
  -- forall * x xs. myAppend * ((:) * x xs) = (.) * * * ((:) * x) (myAppend * xs)

  eval "rhs unfold"

  -- Goal:
  -- forall * x xs. myAppend * ((:) * x xs) = \\ x -> (:) * x (myAppend * xs x)

  eval "lhs (unfold >>> smash)"

  -- Goal:
  -- forall * x xs. \\ ys -> (:) * x (myAppend * xs ys) = \\ x -> (:) * x (myAppend * xs x)

  eval "end-proof -- proven \"repH (:)\""
  eval "rule-to-lemma \"repH ++\""

  -- module main:Main where
  --   absR :: forall a . ([a] -> H a) -> [a] -> [a]
  --   repR :: forall a . ([a] -> [a]) -> [a] -> H a
  --   rev :: forall a . [a] -> [a]
  --   main :: IO ()
  --   main :: IO ()

  eval "prove-lemma \"repH ++\""

  -- Goal:
  -- forall * xs ys. repH * ((++) * xs ys) = (.) * * * (repH * xs) (repH * ys)

  eval "both (any-call (unfold 'repH))"

  -- Goal:
  -- forall * xs ys. (++) * ((++) * xs ys) = (.) * * * ((++) * xs) ((++) * ys)

  eval "both (any-call (unfold-rule appendFix))"

  -- Goal:
  -- forall * xs ys. myAppend * (myAppend * xs ys) = (.) * * * (myAppend * xs) (myAppend * ys)

  eval "lhs (eta-expand 'x)"

  -- Goal:
  -- forall * xs ys. \\ x -> myAppend * (myAppend * xs ys) x = (.) * * * (myAppend * xs) (myAppend * ys)

  eval "rhs unfold"

  -- Goal:
  -- forall * xs ys. \\ x -> myAppend * (myAppend * xs ys) x = \\ x -> myAppend * xs (myAppend * ys x)

  eval "extensionality 'zs"
  eval "simplify"
  eval "{ forall-body"
  eval "  lemma \"myAppend-assoc\""
  eval "}"
  eval "end-proof"

  -- module main:Main where
  --   absR :: forall a . ([a] -> H a) -> [a] -> [a]
  --   repR :: forall a . ([a] -> [a]) -> [a] -> H a
  --   rev :: forall a . [a] -> [a]
  --   main :: IO ()
  --   main :: IO ()

  eval "binding-of 'rev"

  -- rev = \\ * ds ->
  --   case ds of wild *
  --     [] -> [] *
  --     (:) x xs -> (++) * (rev * xs) ((:) * x ([] *))

  eval "fix-intro"

  -- rev = \\ * ->
  --   fix *
  --       (\\ rev ds ->
  --          case ds of wild *
  --            [] -> [] *
  --            (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))

  eval "application-of 'fix"

  -- fix *
  --     (\\ rev ds ->
  --        case ds of wild *
  --          [] -> [] *
  --          (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))

  eval "split-1-beta rev [| absR |] [| repR |]"

  -- Goal:
  -- fix *
  --     ((.) * * * (absR *)
  --          ((.) * * * (repR *)
  --               (\\ rev ds ->
  --                  case ds of wild *
  --                    [] -> [] *
  --                    (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))))
  -- =
  -- fix *
  --     (\\ rev ds ->
  --        case ds of wild *
  --          [] -> [] *
  --          (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))

  eval "both (unfold >>> smash)"

  -- Goal:
  -- let rec x =
  --           absR *
  --                (repR *
  --                      (\\ ds ->
  --                         case ds of wild *
  --                           [] -> [] *
  --                           (:) x xs -> (++) * (x xs) ((:) * x ([] *))))
  -- in x
  -- =
  -- let rec x = \\ ds ->
  --           case ds of wild *
  --             [] -> [] *
  --             (:) x xs -> (++) * (x xs) ((:) * x ([] *))
  -- in x

  eval "lhs (replicate 5 ((one-td unfold) >+> smash))"

  -- Goal:
  -- let rec x = \\ x ->
  --           (++) *
  --                (case x of wild *
  --                   [] -> [] *
  --                   (:) x xs -> (++) * (x xs) ((:) * x ([] *)))
  --                ([] *)
  -- in x
  -- =
  -- let rec x = \\ ds ->
  --           case ds of wild *
  --             [] -> [] *
  --             (:) x xs -> (++) * (x xs) ((:) * x ([] *))
  -- in x

  eval "lhs (one-td (lemma-forward \"++ []\"))"

  -- Goal:
  -- let rec x = \\ x ->
  --           case x of wild *
  --             [] -> [] *
  --             (:) x xs -> (++) * (x xs) ((:) * x ([] *))
  -- in x
  -- =
  -- let rec x = \\ ds ->
  --           case ds of wild *
  --             [] -> [] *
  --             (:) x xs -> (++) * (x xs) ((:) * x ([] *))
  -- in x

  eval "end-proof -- proven rev-assumption"

  -- let g =
  --       (.) * * * (repR *)
  --           ((.) * * *
  --                (\\ rev ds ->
  --                   case ds of wild *
  --                     [] -> [] *
  --                     (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))
  --                (absR *))
  --     worker = fix * g
  -- in absR * worker

  eval "any-call (unfold ['absR,'repR])"

  -- let g =
  --       (.) * * * (\\ eta -> (\\ f -> (.) * * * (repH *) f) eta)
  --           ((.) * * *
  --                (\\ rev ds ->
  --                   case ds of wild *
  --                     [] -> [] *
  --                     (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))
  --                (\\ eta -> (\\ g -> (.) * * * (absH *) g) eta))
  --     worker = fix * g
  -- in (\\ g -> (.) * * * (absH *) g) worker

  eval "repeat (any-call (unfold '.)) ; smash"

  -- let worker =
  --       fix *
  --           (\\ x x ->
  --              repH *
  --                   (case x of wild *
  --                      [] -> [] *
  --                      (:) x xs -> (++) * (absH * (x xs)) ((:) * x ([] *))))
  -- in \\ x -> absH * (worker x)

  eval "one-td (case-float-arg-lemma repHstrict)"

  -- Goal:
  -- forall *. repH * (undefined *) = undefined *

  eval "lhs unfold"

  -- Goal:
  -- forall *. (++) * (undefined *) = undefined *

  eval "lhs (one-td (unfold-rule appendFix))"

  -- Goal:
  -- forall *. myAppend * (undefined *) = undefined *

  eval "lhs unfold"

  -- Goal:
  -- forall *.
  -- \\ ys ->
  --   case undefined * of wild *
  --     [] -> ys
  --     (:) x xs -> (:) * x (myAppend * xs ys)
  -- =
  -- undefined *

  eval "both (innermost undefined-expr)"

  -- Goal:
  -- forall *. undefined * = undefined *

  eval "end-proof -- proven repHstrict"

  -- let worker =
  --       fix *
  --           (\\ x x ->
  --              case x of wild *
  --                [] -> repH * ([] *)
  --                (:) x xs -> repH * ((++) * (absH * (x xs)) ((:) * x ([] *))))
  -- in \\ x -> absH * (worker x)

  eval "one-td (lemma-forward \"repH ++\")"

  -- let worker =
  --       fix *
  --           (\\ x x ->
  --              case x of wild *
  --                [] -> repH * ([] *)
  --                (:) x xs -> (.) * * * (repH * (absH * (x xs))) (repH * ((:) * x ([] *))))
  -- in \\ x -> absH * (worker x)

  eval "repeat (any-call (unfold '.))"

  -- let worker =
  --       fix *
  --           (\\ x x ->
  --              case x of wild *
  --                [] -> repH * ([] *)
  --                (:) x xs -> \\ x -> repH * (absH * (x xs)) (repH * ((:) * x ([] *)) x))
  -- in \\ x -> absH * (worker x)

  eval "one-td (unfold-rule repH-absH-fusion)"

  -- Goal:
  -- forall * h. repH * (absH * h) = h

  eval "assume -- proven repH-absH-fusion"

  -- let worker =
  --       fix *
  --           (\\ x x ->
  --              case x of wild *
  --                [] -> repH * ([] *)
  --                (:) x xs -> \\ x -> x xs (repH * ((:) * x ([] *)) x))
  -- in \\ x -> absH * (worker x)

  eval "one-td (lemma-forward \"repH (:)\")"

  -- let worker =
  --       fix *
  --           (\\ x x ->
  --              case x of wild *
  --                [] -> repH * ([] *)
  --                (:) x xs -> \\ x -> x xs ((.) * * * ((:) * x) (repH * ([] *)) x))
  -- in \\ x -> absH * (worker x)

  eval "any-td (lemma-forward \"repH []\")"

  -- let worker =
  --       fix *
  --           (\\ x x ->
  --              case x of wild *
  --                [] -> id *
  --                (:) x xs -> \\ x -> x xs ((.) * * * ((:) * x) (id *) x))
  -- in \\ x -> absH * (worker x)

  eval "any-call (unfold 'fix)"

  -- let worker =
  --       let rec x =
  --                 (\\ x x ->
  --                    case x of wild *
  --                      [] -> id *
  --                      (:) x xs -> \\ x -> x xs ((.) * * * ((:) * x) (id *) x)) x
  --       in x
  -- in \\ x -> absH * (worker x)

  eval "any-call (unfold 'absH)"

  -- let worker =
  --       let rec x =
  --                 (\\ x x ->
  --                    case x of wild *
  --                      [] -> id *
  --                      (:) x xs -> \\ x -> x xs ((.) * * * ((:) * x) (id *) x)) x
  --       in x
  -- in \\ x -> worker x ([] *)

  eval "bash"

  -- let rec x = \\ x ->
  --           case x of wild *
  --             [] -> \\ x -> x
  --             (:) x xs -> \\ x -> x xs ((:) * x x)
  -- in \\ x -> x x ([] *)

  eval "unshadow"

  -- let rec x = \\ x0 ->
  --           case x0 of wild *
  --             [] -> \\ x1 -> x1
  --             (:) x1 xs -> \\ x2 -> x xs ((:) * x1 x2)
  -- in \\ x0 -> x x0 ([] *)


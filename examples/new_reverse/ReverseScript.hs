import HERMIT.API
script :: Shell ()
script = do
  eval "flatten-module"
  eval "rule-to-lemma \"++ []\""

  eval "-- module main:Main where"
  eval "--   absR :: forall a . ([a] -> H a) -> [a] -> [a]"
  eval "--   repR :: forall a . ([a] -> [a]) -> [a] -> H a"
  eval "--   rev :: forall a . [a] -> [a]"
  eval "--   main :: IO ()"
  eval "--   main :: IO ()"

  eval "prove-lemma \"++ []\""

  eval "-- Goal:"
  eval "-- forall * xs. (++) * xs ([] *) = xs"

  eval "lhs (one-td (unfold-rule appendFix))"

  eval "-- Goal:"
  eval "-- forall *. (++) * = myAppend *"

  eval "assume -- proven appendFix"

  eval "-- Goal:"
  eval "-- forall * xs. myAppend * xs ([] *) = xs"

  eval "induction 'xs"

  eval "-- Goal:"
  eval "-- forall *. myAppend * (undefined *) ([] *) = undefined *"

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

  eval "-- module main:Main where"
  eval "--   absR :: forall a . ([a] -> H a) -> [a] -> [a]"
  eval "--   repR :: forall a . ([a] -> [a]) -> [a] -> H a"
  eval "--   rev :: forall a . [a] -> [a]"
  eval "--   main :: IO ()"
  eval "--   main :: IO ()"

  eval "prove-lemma \"repH []\""

  eval "-- Goal:"
  eval "-- forall *. repH * ([] *) = id *"

  eval "lhs unfold"

  eval "-- Goal:"
  eval "-- forall *. (++) * ([] *) = id *"

  eval "extensionality"

  eval "-- Goal:"
  eval "-- forall * x. (++) * ([] *) x = id * x"

  eval "lhs (one-td (unfold-rule appendFix))"

  eval "-- Goal:"
  eval "-- forall * x. myAppend * ([] *) x = id * x"

  eval "lhs unfold"

  eval "-- Goal:"
  eval "-- forall * x."
  eval "-- case [] * of wild *"
  eval "--   [] -> x"
  eval "--   (:) x xs -> (:) * x (myAppend * xs x)"
  eval "-- ="
  eval "-- id * x"

  eval "both smash"

  eval "-- Goal:"
  eval "-- forall * x. x = x"

  eval "end-proof -- proven \"repH []\""
  eval "rule-to-lemma \"repH (:)\""

  eval "-- module main:Main where"
  eval "--   absR :: forall a . ([a] -> H a) -> [a] -> [a]"
  eval "--   repR :: forall a . ([a] -> [a]) -> [a] -> H a"
  eval "--   rev :: forall a . [a] -> [a]"
  eval "--   main :: IO ()"
  eval "--   main :: IO ()"

  eval "prove-lemma \"repH (:)\""

  eval "-- Goal:"
  eval "-- forall * x xs. repH * ((:) * x xs) = (.) * * * ((:) * x) (repH * xs)"

  eval "both (any-call (unfold 'repH))"

  eval "-- Goal:"
  eval "-- forall * x xs. (++) * ((:) * x xs) = (.) * * * ((:) * x) ((++) * xs)"

  eval "both (any-call (unfold-rule appendFix))"

  eval "-- Goal:"
  eval "-- forall * x xs. myAppend * ((:) * x xs) = (.) * * * ((:) * x) (myAppend * xs)"

  eval "rhs unfold"

  eval "-- Goal:"
  eval "-- forall * x xs. myAppend * ((:) * x xs) = \\ x -> (:) * x (myAppend * xs x)"

  eval "lhs (unfold >>> smash)"

  eval "-- Goal:"
  eval "-- forall * x xs. \\ ys -> (:) * x (myAppend * xs ys) = \\ x -> (:) * x (myAppend * xs x)"

  eval "end-proof -- proven \"repH (:)\""
  eval "rule-to-lemma \"repH ++\""

  eval "-- module main:Main where"
  eval "--   absR :: forall a . ([a] -> H a) -> [a] -> [a]"
  eval "--   repR :: forall a . ([a] -> [a]) -> [a] -> H a"
  eval "--   rev :: forall a . [a] -> [a]"
  eval "--   main :: IO ()"
  eval "--   main :: IO ()"

  eval "prove-lemma \"repH ++\""

  eval "-- Goal:"
  eval "-- forall * xs ys. repH * ((++) * xs ys) = (.) * * * (repH * xs) (repH * ys)"

  eval "both (any-call (unfold 'repH))"

  eval "-- Goal:"
  eval "-- forall * xs ys. (++) * ((++) * xs ys) = (.) * * * ((++) * xs) ((++) * ys)"

  eval "both (any-call (unfold-rule appendFix))"

  eval "-- Goal:"
  eval "-- forall * xs ys. myAppend * (myAppend * xs ys) = (.) * * * (myAppend * xs) (myAppend * ys)"

  eval "lhs (eta-expand 'x)"

  eval "-- Goal:"
  eval "-- forall * xs ys. \\ x -> myAppend * (myAppend * xs ys) x = (.) * * * (myAppend * xs) (myAppend * ys)"

  eval "rhs unfold"

  eval "-- Goal:"
  eval "-- forall * xs ys. \\ x -> myAppend * (myAppend * xs ys) x = \\ x -> myAppend * xs (myAppend * ys x)"

  eval "extensionality 'zs"
  eval "simplify"
  eval "{ forall-body"
  eval "  lemma \"myAppend-assoc\""
  eval "}"
  eval "end-proof"

  eval "-- module main:Main where"
  eval "--   absR :: forall a . ([a] -> H a) -> [a] -> [a]"
  eval "--   repR :: forall a . ([a] -> [a]) -> [a] -> H a"
  eval "--   rev :: forall a . [a] -> [a]"
  eval "--   main :: IO ()"
  eval "--   main :: IO ()"

  eval "binding-of 'rev"

  eval "-- rev = \\ * ds ->"
  eval "--   case ds of wild *"
  eval "--     [] -> [] *"
  eval "--     (:) x xs -> (++) * (rev * xs) ((:) * x ([] *))"

  eval "fix-intro"

  eval "-- rev = \\ * ->"
  eval "--   fix *"
  eval "--       (\\ rev ds ->"
  eval "--          case ds of wild *"
  eval "--            [] -> [] *"
  eval "--            (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))"

  eval "application-of 'fix"

  eval "-- fix *"
  eval "--     (\\ rev ds ->"
  eval "--        case ds of wild *"
  eval "--          [] -> [] *"
  eval "--          (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))"

  eval "split-1-beta rev [| absR |] [| repR |]"

  eval "-- Goal:"
  eval "-- fix *"
  eval "--     ((.) * * * (absR *)"
  eval "--          ((.) * * * (repR *)"
  eval "--               (\\ rev ds ->"
  eval "--                  case ds of wild *"
  eval "--                    [] -> [] *"
  eval "--                    (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))))"
  eval "-- ="
  eval "-- fix *"
  eval "--     (\\ rev ds ->"
  eval "--        case ds of wild *"
  eval "--          [] -> [] *"
  eval "--          (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))"

  eval "both (unfold >>> smash)"

  eval "-- Goal:"
  eval "-- let rec x ="
  eval "--           absR *"
  eval "--                (repR *"
  eval "--                      (\\ ds ->"
  eval "--                         case ds of wild *"
  eval "--                           [] -> [] *"
  eval "--                           (:) x xs -> (++) * (x xs) ((:) * x ([] *))))"
  eval "-- in x"
  eval "-- ="
  eval "-- let rec x = \\ ds ->"
  eval "--           case ds of wild *"
  eval "--             [] -> [] *"
  eval "--             (:) x xs -> (++) * (x xs) ((:) * x ([] *))"
  eval "-- in x"

  eval "lhs (replicate 5 ((one-td unfold) >+> smash))"

  eval "-- Goal:"
  eval "-- let rec x = \\ x ->"
  eval "--           (++) *"
  eval "--                (case x of wild *"
  eval "--                   [] -> [] *"
  eval "--                   (:) x xs -> (++) * (x xs) ((:) * x ([] *)))"
  eval "--                ([] *)"
  eval "-- in x"
  eval "-- ="
  eval "-- let rec x = \\ ds ->"
  eval "--           case ds of wild *"
  eval "--             [] -> [] *"
  eval "--             (:) x xs -> (++) * (x xs) ((:) * x ([] *))"
  eval "-- in x"

  eval "lhs (one-td (lemma-forward \"++ []\"))"

  eval "-- Goal:"
  eval "-- let rec x = \\ x ->"
  eval "--           case x of wild *"
  eval "--             [] -> [] *"
  eval "--             (:) x xs -> (++) * (x xs) ((:) * x ([] *))"
  eval "-- in x"
  eval "-- ="
  eval "-- let rec x = \\ ds ->"
  eval "--           case ds of wild *"
  eval "--             [] -> [] *"
  eval "--             (:) x xs -> (++) * (x xs) ((:) * x ([] *))"
  eval "-- in x"

  eval "end-proof -- proven rev-assumption"

  eval "-- let g ="
  eval "--       (.) * * * (repR *)"
  eval "--           ((.) * * *"
  eval "--                (\\ rev ds ->"
  eval "--                   case ds of wild *"
  eval "--                     [] -> [] *"
  eval "--                     (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))"
  eval "--                (absR *))"
  eval "--     worker = fix * g"
  eval "-- in absR * worker"

  eval "any-call (unfold ['absR,'repR])"

  eval "-- let g ="
  eval "--       (.) * * * (\\ eta -> (\\ f -> (.) * * * (repH *) f) eta)"
  eval "--           ((.) * * *"
  eval "--                (\\ rev ds ->"
  eval "--                   case ds of wild *"
  eval "--                     [] -> [] *"
  eval "--                     (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))"
  eval "--                (\\ eta -> (\\ g -> (.) * * * (absH *) g) eta))"
  eval "--     worker = fix * g"
  eval "-- in (\\ g -> (.) * * * (absH *) g) worker"

  eval "repeat (any-call (unfold '.)) ; smash"

  eval "-- let worker ="
  eval "--       fix *"
  eval "--           (\\ x x ->"
  eval "--              repH *"
  eval "--                   (case x of wild *"
  eval "--                      [] -> [] *"
  eval "--                      (:) x xs -> (++) * (absH * (x xs)) ((:) * x ([] *))))"
  eval "-- in \\ x -> absH * (worker x)"

  eval "one-td (case-float-arg-lemma repHstrict)"

  eval "-- Goal:"
  eval "-- forall *. repH * (undefined *) = undefined *"

  eval "lhs unfold"

  eval "-- Goal:"
  eval "-- forall *. (++) * (undefined *) = undefined *"

  eval "lhs (one-td (unfold-rule appendFix))"

  eval "-- Goal:"
  eval "-- forall *. myAppend * (undefined *) = undefined *"

  eval "lhs unfold"

  eval "-- Goal:"
  eval "-- forall *."
  eval "-- \\ ys ->"
  eval "--   case undefined * of wild *"
  eval "--     [] -> ys"
  eval "--     (:) x xs -> (:) * x (myAppend * xs ys)"
  eval "-- ="
  eval "-- undefined *"

  eval "both (innermost undefined-expr)"

  eval "-- Goal:"
  eval "-- forall *. undefined * = undefined *"

  eval "end-proof -- proven repHstrict"

  eval "-- let worker ="
  eval "--       fix *"
  eval "--           (\\ x x ->"
  eval "--              case x of wild *"
  eval "--                [] -> repH * ([] *)"
  eval "--                (:) x xs -> repH * ((++) * (absH * (x xs)) ((:) * x ([] *))))"
  eval "-- in \\ x -> absH * (worker x)"

  eval "one-td (lemma-forward \"repH ++\")"

  eval "-- let worker ="
  eval "--       fix *"
  eval "--           (\\ x x ->"
  eval "--              case x of wild *"
  eval "--                [] -> repH * ([] *)"
  eval "--                (:) x xs -> (.) * * * (repH * (absH * (x xs))) (repH * ((:) * x ([] *))))"
  eval "-- in \\ x -> absH * (worker x)"

  eval "repeat (any-call (unfold '.))"

  eval "-- let worker ="
  eval "--       fix *"
  eval "--           (\\ x x ->"
  eval "--              case x of wild *"
  eval "--                [] -> repH * ([] *)"
  eval "--                (:) x xs -> \\ x -> repH * (absH * (x xs)) (repH * ((:) * x ([] *)) x))"
  eval "-- in \\ x -> absH * (worker x)"

  eval "one-td (unfold-rule repH-absH-fusion)"

  eval "-- Goal:"
  eval "-- forall * h. repH * (absH * h) = h"

  eval "assume -- proven repH-absH-fusion"

  eval "-- let worker ="
  eval "--       fix *"
  eval "--           (\\ x x ->"
  eval "--              case x of wild *"
  eval "--                [] -> repH * ([] *)"
  eval "--                (:) x xs -> \\ x -> x xs (repH * ((:) * x ([] *)) x))"
  eval "-- in \\ x -> absH * (worker x)"

  eval "one-td (lemma-forward \"repH (:)\")"

  eval "-- let worker ="
  eval "--       fix *"
  eval "--           (\\ x x ->"
  eval "--              case x of wild *"
  eval "--                [] -> repH * ([] *)"
  eval "--                (:) x xs -> \\ x -> x xs ((.) * * * ((:) * x) (repH * ([] *)) x))"
  eval "-- in \\ x -> absH * (worker x)"

  eval "any-td (lemma-forward \"repH []\")"

  eval "-- let worker ="
  eval "--       fix *"
  eval "--           (\\ x x ->"
  eval "--              case x of wild *"
  eval "--                [] -> id *"
  eval "--                (:) x xs -> \\ x -> x xs ((.) * * * ((:) * x) (id *) x))"
  eval "-- in \\ x -> absH * (worker x)"

  eval "any-call (unfold 'fix)"

  eval "-- let worker ="
  eval "--       let rec x ="
  eval "--                 (\\ x x ->"
  eval "--                    case x of wild *"
  eval "--                      [] -> id *"
  eval "--                      (:) x xs -> \\ x -> x xs ((.) * * * ((:) * x) (id *) x)) x"
  eval "--       in x"
  eval "-- in \\ x -> absH * (worker x)"

  eval "any-call (unfold 'absH)"

  eval "-- let worker ="
  eval "--       let rec x ="
  eval "--                 (\\ x x ->"
  eval "--                    case x of wild *"
  eval "--                      [] -> id *"
  eval "--                      (:) x xs -> \\ x -> x xs ((.) * * * ((:) * x) (id *) x)) x"
  eval "--       in x"
  eval "-- in \\ x -> worker x ([] *)"

  eval "bash"

  eval "-- let rec x = \\ x ->"
  eval "--           case x of wild *"
  eval "--             [] -> \\ x -> x"
  eval "--             (:) x xs -> \\ x -> x xs ((:) * x x)"
  eval "-- in \\ x -> x x ([] *)"

  eval "unshadow"

  eval "-- let rec x = \\ x0 ->"
  eval "--           case x0 of wild *"
  eval "--             [] -> \\ x1 -> x1"
  eval "--             (:) x1 xs -> \\ x2 -> x xs ((:) * x1 x2)"
  eval "-- in \\ x0 -> x x0 ([] *)"



import Prelude hiding (repeat)

import HERMIT.API

script :: Shell ()
script = do
  apply flattenModule
  eval "rule-to-lemma \"++ []\""

  -- module main:Main where
  --   absR :: forall a . ([a] -> H a) -> [a] -> [a]
  --   repR :: forall a . ([a] -> [a]) -> [a] -> H a
  --   rev :: forall a . [a] -> [a]
  --   main :: IO ()
  --   main :: IO ()

  shellEffect $ proveLemma "++ []"

  -- Goal:
  -- forall * xs. (++) * xs ([] *) = xs

  eval "lhs (one-td (unfold-rule appendFix))"

  -- Goal:
  -- forall *. (++) * = myAppend *

  proofCmd assume

  -- Goal:
  -- forall * xs. myAppend * xs ([] *) = xs

  apply $ induction "xs"

  -- Goal:
  -- forall *. myAppend * (undefined *) ([] *) = undefined *

  apply $ anyBU (unfoldWith "myAppend" >>> undefinedExpr)
  apply $ anyBU (unfoldWith "myAppend" >>> caseReduce)
  apply simplifyLemma
  sendCrumb forallBody ; sendCrumb consequent
  apply $ oneTD (lemmaForward "ind-hyp-0")
  eval "end-case"

  eval "rule-to-lemma \"myAppend-assoc\""
  shellEffect $ proveLemma "myAppend-assoc"
  apply $ induction "xs"
  scope $ do sendCrumb forallBody

             scope $ do sendCrumb conjLhs
                        apply $ anyBU ((unfoldWith "myAppend") >>> undefinedCase)
                        apply reflexivity

             sendCrumb conjRhs

             scope $ do sendCrumb conjLhs
                        apply $ anyBU ((unfoldWith "myAppend") >>> caseReduce)
                        apply reflexivity

             scope $ do sendCrumb conjRhs
                        sendCrumb forallBody ; sendCrumb consequent
                        apply $ anyBU (unfoldWith "myAppend")
                        apply smash
                        apply $ rhsR (oneTD (fold "myAppend"))
                        apply (oneTD (lemmaForward "ind-hyp-0"))
                        apply reflexivity
  eval "end-proof"

  eval "rule-to-lemma \"repH []\""

  -- module main:Main where
  --   absR :: forall a . ([a] -> H a) -> [a] -> [a]
  --   repR :: forall a . ([a] -> [a]) -> [a] -> H a
  --   rev :: forall a . [a] -> [a]
  --   main :: IO ()
  --   main :: IO ()

  shellEffect $ proveLemma "repH []"

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

  shellEffect $ proveLemma "repH (:)"

  -- Goal:
  -- forall * x xs. repH * ((:) * x xs) = (.) * * * ((:) * x) (repH * xs)

  eval "both (any-call (unfold 'repH))"

  -- Goal:
  -- forall * x xs. (++) * ((:) * x xs) = (.) * * * ((:) * x) ((++) * xs)

  eval "both (any-call (unfold-rule appendFix))"

  -- Goal:
  -- forall * x xs. myAppend * ((:) * x xs) = (.) * * * ((:) * x) (myAppend * xs)

  apply $ rhsR unfold

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

  shellEffect $ proveLemma "repH ++"

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

  apply $ rhsR unfold

  -- Goal:
  -- forall * xs ys. \\ x -> myAppend * (myAppend * xs ys) x = \\ x -> myAppend * xs (myAppend * ys x)

  eval "extensionality 'zs"
  apply simplify
  scope $ do sendCrumb forallBody
             apply $ lemma "myAppend-assoc"
  eval "end-proof"

  -- module main:Main where
  --   absR :: forall a . ([a] -> H a) -> [a] -> [a]
  --   repR :: forall a . ([a] -> [a]) -> [a] -> H a
  --   rev :: forall a . [a] -> [a]
  --   main :: IO ()
  --   main :: IO ()

  setPath $ bindingOf "rev"

  -- rev = \\ * ds ->
  --   case ds of wild *
  --     [] -> [] *
  --     (:) x xs -> (++) * (rev * xs) ((:) * x ([] *))

  apply fixIntro

  -- rev = \\ * ->
  --   fix *
  --       (\\ rev ds ->
  --          case ds of wild *
  --            [] -> [] *
  --            (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))

  setPath $ applicationOf "fix"

  -- fix *
  --     (\\ rev ds ->
  --        case ds of wild *
  --          [] -> [] *
  --          (:) x xs -> (++) * (rev xs) ((:) * x ([] *)))

  apply $ split1Beta "rev" "absR" "repR"

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

  apply $ anyCall (unfoldAny ["absR","repR"])

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

  apply $ repeat (anyCall (unfoldWith ".")) ; apply smash

  -- let worker =
  --       fix *
  --           (\\ x x ->
  --              repH *
  --                   (case x of wild *
  --                      [] -> [] *
  --                      (:) x xs -> (++) * (absH * (x xs)) ((:) * x ([] *))))
  -- in \\ x -> absH * (worker x)

  apply $ oneTD (caseFloatArgLemma "repHstrict")

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

  apply $ repeat (anyCall (unfoldWith "."))

  -- let worker =
  --       fix *
  --           (\\ x x ->
  --              case x of wild *
  --                [] -> repH * ([] *)
  --                (:) x xs -> \\ x -> repH * (absH * (x xs)) (repH * ((:) * x ([] *)) x))
  -- in \\ x -> absH * (worker x)

  apply $ oneTD (unfoldRule "repH-absH-fusion")

  -- Goal:
  -- forall * h. repH * (absH * h) = h

  proofCmd assume -- proven repH-absH-fusion

  -- let worker =
  --       fix *
  --           (\\ x x ->
  --              case x of wild *
  --                [] -> repH * ([] *)
  --                (:) x xs -> \\ x -> x xs (repH * ((:) * x ([] *)) x))
  -- in \\ x -> absH * (worker x)

  apply $ oneTD (lemmaForward "repH (:)")

  -- let worker =
  --       fix *
  --           (\\ x x ->
  --              case x of wild *
  --                [] -> repH * ([] *)
  --                (:) x xs -> \\ x -> x xs ((.) * * * ((:) * x) (repH * ([] *)) x))
  -- in \\ x -> absH * (worker x)

  apply $ anyTD (lemmaForward "repH []")

  -- let worker =
  --       fix *
  --           (\\ x x ->
  --              case x of wild *
  --                [] -> id *
  --                (:) x xs -> \\ x -> x xs ((.) * * * ((:) * x) (id *) x))
  -- in \\ x -> absH * (worker x)

  apply $ anyCall (unfoldWith "fix")

  -- let worker =
  --       let rec x =
  --                 (\\ x x ->
  --                    case x of wild *
  --                      [] -> id *
  --                      (:) x xs -> \\ x -> x xs ((.) * * * ((:) * x) (id *) x)) x
  --       in x
  -- in \\ x -> absH * (worker x)

  apply $ anyCall (unfoldWith "absH")

  -- let worker =
  --       let rec x =
  --                 (\\ x x ->
  --                    case x of wild *
  --                      [] -> id *
  --                      (:) x xs -> \\ x -> x xs ((.) * * * ((:) * x) (id *) x)) x
  --       in x
  -- in \\ x -> worker x ([] *)

  apply bash

  -- let rec x = \\ x ->
  --           case x of wild *
  --             [] -> \\ x -> x
  --             (:) x xs -> \\ x -> x xs ((:) * x x)
  -- in \\ x -> x x ([] *)

  apply unshadow

  -- let rec x = \\ x0 ->
  --           case x0 of wild *
  --             [] -> \\ x1 -> x1
  --             (:) x1 xs -> \\ x2 -> x xs ((:) * x1 x2)
  -- in \\ x0 -> x x0 ([] *)


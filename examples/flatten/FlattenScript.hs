import HERMIT.API
script :: Shell ()
script = do
  eval "load-as-rewrite \"WWA\" \"WW-Ass-A.hss\""
  eval "define-rewrite \"WWC\" \"ww-result-AssA-to-AssC WWA\""
  eval "load-as-rewrite \"StrictRepH\" \"StrictRepH.hss\""

  eval "-- module main:Main where"
  eval "--   flatten :: forall a . Tree a -> [a]"
  eval "--   $dShow :: Show [Char]"
  eval "--   main :: IO ()"
  eval "--   main :: IO ()"

  eval "binding-of 'flatten"

  eval "-- flatten = \\ * ds ->"
  eval "--   case ds of wild *"
  eval "--     Node l r -> (++) * (flatten * l) (flatten * r)"
  eval "--     Leaf a -> (:) * a ([] *)"

  eval "ww-result-split-static-arg 1 [0] [| absH |] [| repH |] WWC"

  eval "-- flatten = \\ * ds ->"
  eval "--   (let f = \\ flatten' ds ->"
  eval "--          case ds of wild *"
  eval "--            Node l r -> (++) * (flatten' l) (flatten' r)"
  eval "--            Leaf a -> (:) * a ([] *)"
  eval "--        rec work = \\ x1 -> repH * (f (\\ x2 -> absH * (work x2)) x1)"
  eval "--    in \\ x0 -> absH * (work x0)) ds"

  eval "bash"
  eval "{"

  eval "-- flatten = \\ * ->"
  eval "--   let rec work = \\ x1 ->"
  eval "--             repH *"
  eval "--                  (case x1 of wild *"
  eval "--                     Node l r -> (++) * (absH * (work l)) (absH * (work r))"
  eval "--                     Leaf a -> (:) * a ([] *))"
  eval "--   in \\ x0 -> absH * (work x0)"

  eval "rhs-of 'work"

  eval "-- \\ x1 ->"
  eval "--   repH *"
  eval "--        (case x1 of wild *"
  eval "--           Node l r -> (++) * (absH * (work l)) (absH * (work r))"
  eval "--           Leaf a -> (:) * a ([] *))"

  eval "alpha-lam 'tree"

  eval "-- \\ tree ->"
  eval "--   repH *"
  eval "--        (case tree of wild *"
  eval "--           Node l r -> (++) * (absH * (work l)) (absH * (work r))"
  eval "--           Leaf a -> (:) * a ([] *))"

  eval "lam-body"

  eval "-- repH *"
  eval "--      (case tree of wild *"
  eval "--         Node l r -> (++) * (absH * (work l)) (absH * (work r))"
  eval "--         Leaf a -> (:) * a ([] *))"

  eval "eta-expand 'acc"

  eval "-- \\ acc ->"
  eval "--   repH *"
  eval "--        (case tree of wild *"
  eval "--           Node l r -> (++) * (absH * (work l)) (absH * (work r))"
  eval "--           Leaf a -> (:) * a ([] *))"
  eval "--        acc"

  eval "lam-body"

  eval "-- repH *"
  eval "--      (case tree of wild *"
  eval "--         Node l r -> (++) * (absH * (work l)) (absH * (work r))"
  eval "--         Leaf a -> (:) * a ([] *))"
  eval "--      acc"

  eval "bash-extended-with [push 'repH StrictRepH,forward ww-result-fusion,unfold-rules-unsafe [\"repH ++\",\"repH (:)\",\"repH []\"]]"

  eval "-- case tree of wild *"
  eval "--   Node l r -> work l (work r acc)"
  eval "--   Leaf a -> (:) * a acc"

  eval " }"

  eval "-- flatten = \\ * ->"
  eval "--   let rec work = \\ tree acc ->"
  eval "--             case tree of wild *"
  eval "--               Node l r -> work l (work r acc)"
  eval "--               Leaf a -> (:) * a acc"
  eval "--   in \\ x0 -> absH * (work x0)"

  eval "one-td (unfold 'absH)"

  eval "-- flatten = \\ * ->"
  eval "--   let rec work = \\ tree acc ->"
  eval "--             case tree of wild *"
  eval "--               Node l r -> work l (work r acc)"
  eval "--               Leaf a -> (:) * a acc"
  eval "--   in \\ x0 -> work x0 ([] *)"

  -- Assuming unproven lemmas:
  unprovenAssume "++ []"
  unprovenAssume "++ strict"
  unprovenAssume "repH (:)"
  unprovenAssume "repH ++"
  unprovenAssume "repH []"

unprovenAssume :: String -> Shell ()
unprovenAssume lemmaName = do
  eval $ "prove-lemma " ++ show lemmaName
  proofCmd assume



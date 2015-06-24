module FlattenScript where
import HERMIT.API
import HERMIT.API.Types

import StrictRepHScript
import WWAssAScript

script :: Shell ()
script = do
  -- module main:Main where
  --   flatten :: forall a . Tree a -> [a]
  --   $dShow :: Show [Char]
  --   main :: IO ()
  --   main :: IO ()

  setPath (bindingOf "flatten")

  -- flatten = \\ * ds ->
  --   case ds of wild *
  --     Node l r -> (++) * (flatten * l) (flatten * r)
  --     Leaf a -> (:) * a ([] *)

  apply $ wwResultSplitStaticArg 1 [0] "absH" "repH" (wwResultAssAToAssC wwa)

  -- flatten = \\ * ds ->
  --   (let f = \\ flatten' ds ->
  --          case ds of wild *
  --            Node l r -> (++) * (flatten' l) (flatten' r)
  --            Leaf a -> (:) * a ([] *)
  --        rec work = \\ x1 -> repH * (f (\\ x2 -> absH * (work x2)) x1)
  --    in \\ x0 -> absH * (work x0)) ds

  apply bash
  scope $ do
    -- flatten = \\ * ->
    --   let rec work = \\ x1 ->
    --             repH *
    --                  (case x1 of wild *
    --                     Node l r -> (++) * (absH * (work l)) (absH * (work r))
    --                     Leaf a -> (:) * a ([] *))
    --   in \\ x0 -> absH * (work x0)

    setPath (rhsOf "work")

    -- \\ x1 ->
    --   repH *
    --        (case x1 of wild *
    --           Node l r -> (++) * (absH * (work l)) (absH * (work r))
    --           Leaf a -> (:) * a ([] *))

    apply $ alphaLamWith "tree"

    -- \\ tree ->
    --   repH *
    --        (case tree of wild *
    --           Node l r -> (++) * (absH * (work l)) (absH * (work r))
    --           Leaf a -> (:) * a ([] *))

    sendCrumb lamBody

    -- repH *
    --      (case tree of wild *
    --         Node l r -> (++) * (absH * (work l)) (absH * (work r))
    --         Leaf a -> (:) * a ([] *))

    apply $ etaExpand "acc"

    -- \\ acc ->
    --   repH *
    --        (case tree of wild *
    --           Node l r -> (++) * (absH * (work l)) (absH * (work r))
    --           Leaf a -> (:) * a ([] *))
    --        acc

    sendCrumb lamBody

    -- repH *
    --      (case tree of wild *
    --         Node l r -> (++) * (absH * (work l)) (absH * (work r))
    --         Leaf a -> (:) * a ([] *))
    --      acc

    apply $ bashExtendedWith [ push "repH" strictRepH, forward wwResultFusion, unfoldRulesUnsafe ["repH ++", "repH (:)", "repH []"] ]

    -- case tree of wild *
    --   Node l r -> work l (work r acc)
    --   Leaf a -> (:) * a acc

  -- flatten = \\ * ->
  --   let rec work = \\ tree acc ->
  --             case tree of wild *
  --               Node l r -> work l (work r acc)
  --               Leaf a -> (:) * a acc
  --   in \\ x0 -> absH * (work x0)

  apply $ oneTD (unfoldWith "absH")

  -- flatten = \\ * ->
  --   let rec work = \\ tree acc ->
  --             case tree of wild *
  --               Node l r -> work l (work r acc)
  --               Leaf a -> (:) * a acc
  --   in \\ x0 -> work x0 ([] *)

  -- Assuming unproven lemmas:
  unprovenAssume "++ []"
  unprovenAssume "++ strict"
  unprovenAssume "repH (:)"
  unprovenAssume "repH ++"
  unprovenAssume "repH []"

unprovenAssume :: LemmaName -> Shell ()
unprovenAssume lemmaName = do
  shellEffect $ proveLemma lemmaName
  proofCmd assume


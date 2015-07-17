{-# LANGUAGE NoImplicitPrelude #-}
module VerifyListMonadAssocScript (listMonadAssoc) where
--------------------------------------------
-- Verify monad-assoc (for List)
--
--
-- forall m f g.  (m `bind` f) `bind` g  =  m `bind` \x -> (f x `bind` g)

-- OR, using Kleisli composition
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- (m >=> n) x = m x >>= n
--
-- Associativity: (f >=> g) >=> h ≡ f >=> (g >=> h)
--
--
--
-- TO BE COMPLETED.
--
--
-- See:  http://mvanier.livejournal.com/4647.html
-- "Yet Another Monad Tutorial (part 4: The Maybe and List Monads)"
-- by Mike Vanier
-- for a clear and detailed proof of this monad law for List.
--
-- He factors this problem (in a way convenient for us) with:
-- "In addition, I'll be using several identities involving map and concat applied to lists.
--  You should just take these on faith for now, though I'll show how to derive them below._
--  -- equation 1:
--  map (f . g)  =  map f . map g
--  -- equation 2:
--  map f . concat =  concat . map (map f)
--  \f x -> map f (concat x) == \f x -> concat (map (map f) x)
--  -- equation 3:
--  concat . concat  =  concat . map concato
-- \x -> concat (concat x) == \x -> concat (map concat x)

--------------------------------------------
import HERMIT.API.Prelude

import VerifyMapComposeScript
import VerifyConcatNonemptyScript
import VerifyConcatConcatScript
import VerifyMapConcatScript

script :: Shell ()
script = listMonadAssoc

assocLhs :: Rewrite LCore
assocLhs
  = serialise
      [ anyCall (unfoldWith "bind")
      , anyCall (unfoldWith "bind")
      , anyBU (lemmaForward "map-concat")
      , anyBU (lemmaForward "concat-concat")
      ]

listMonadAssoc :: Shell ()
listMonadAssoc = do
  mapCompose
  concatNonempty
  concatConcat
  mapConcat

  eval "rule-to-lemma monad-assoc"

  proof "monad-assoc" $ do
    pathS [forallBody] $ do
      apply . lhsR $ assocLhs

      pathS [eqRhs] $ do
        apply . anyCall $ unfoldWith "bind"
        pathS [appArg, appFun, appArg, lamBody, appArg] $ do
          apply $ fold "."
        pathS [appArg, appFun, appArg] $ do
          apply . anyBU $ fold "."
          apply etaReduce
        apply . oneBU $ lemmaForward "map-compose"
        apply smash
        apply . oneBU $ lemmaForward "map-compose"

      apply reflexivity


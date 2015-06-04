{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Local.Let where

import Data.Aeson
import HERMIT.API.Types

-- | Let substitution: (let x = e1 in e2) ==> (e2[e1/x])
-- x must not be free in e1.
letSubst :: Rewrite LCore
letSubst = Transform $ method "letSubst" []

-- | Safe let substitution
-- let x = e1 in e2, safe to inline without duplicating work ==> e2[e1/x],
-- x must not be free in e1.
letSubstSafe :: Rewrite LCore
letSubstSafe = Transform $ method "letSubstSafe" []

-- | As let-subst-safe, but does not try to convert a recursive let into a non-recursive let first.
letNonrecSubstSafe :: Rewrite LCore
letNonrecSubstSafe = Transform $ method "letNonrecSubstSafe" []

-- | e => (let v = e in v), name of v is provided
letIntro :: String -> Rewrite LCore
letIntro str = Transform $ method "letIntro" [toJSON str]

-- | e => let f' = defn[f'/f] in e[f'/f], name of f is provided
letIntroUnfolding :: Name -> Rewrite LCore
letIntroUnfolding nm = Transform $ method "letIntroUnfolding" [toJSON nm]

-- | Remove an unused let binding.
-- (let v = e1 in e2) ==> e2, if v is not free in e1 or e2.
letElim :: Rewrite LCore
letElim = Transform $ method "letElim" []

-- | (let v = ev in e) x ==> let v = ev in e x
letFloatApp :: Rewrite LCore
letFloatApp = Transform $ method "letFloatApp" []

-- | f (let v = ev in e) ==> let v = ev in f e
letFloatArg :: Rewrite LCore
letFloatArg = Transform $ method "letFloatArg" []

-- | The Full Laziness Transformation
-- (\\ v1 -> let v2 = e1 in e2)  ==>  let v2 = e1 in (\\ v1 -> e2), if v1 is not free in e2.
-- If v1 = v2 then v1 will be alpha-renamed.
letFloatLam :: Rewrite LCore
letFloatLam = Transform $ method "letFloatLam" []

-- | let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e
letFloatLet :: Rewrite LCore
letFloatLet = Transform $ method "letFloatLet" []

-- | case (let v = ev in e) of ... ==> let v = ev in case e of ...
letFloatCase :: Rewrite LCore
letFloatCase = Transform $ method "letFloatCase" []

-- -- | case s of { ... ; p -> let v = ev in e ; ... }
-- -- ==> let v = ev in case s of { ... ; p -> e ; ... }
-- letFloatCaseAlt :: Rewrite LCore
--
-- -- | Float a let binding from specified alternative.
-- -- case s of { ... ; p -> let v = ev in e ; ... }
-- -- ==> let v = ev in case s of { ... ; p -> e ; ... }
-- letFloatCaseAlt :: Int -> Rewrite LCore

-- | cast (let bnds in e) co ==> let bnds in cast e co
letFloatCast :: Rewrite LCore
letFloatCast = Transform $ method "letFloatCast" []

-- | v = (let bds in e) : prog ==> bds : v = e : prog
letFloatTop :: Rewrite LCore
letFloatTop = Transform $ method "letFloatTop" []

-- | Float a Let whatever the context.
letFloat :: Rewrite LCore
letFloat = Transform $ method "letFloat" []

-- | let v = ev in e ==> case ev of v -> e
letToCase :: Rewrite LCore
letToCase = Transform $ method "letToCase" []

-- | Float-in a let if possible.
letFloatIn :: Rewrite LCore
letFloatIn = Transform $ method "letFloatIn" []

-- | let v = ev in f a ==> (let v = ev in f) (let v = ev in a)
letFloatInApp :: Rewrite LCore
letFloatInApp = Transform $ method "letFloatInApp" []

-- | let v = ev in case s of p -> e ==> case (let v = ev in s) of p -> let v = ev in e
-- if v does not shadow a pattern binder in p
letFloatInCase :: Rewrite LCore
letFloatInCase = Transform $ method "letFloatInCase" []

-- | let v = ev in \\ x -> e ==> \\ x -> let v = ev in e
-- if v does not shadow x
letFloatInLam :: Rewrite LCore
letFloatInLam = Transform $ method "letFloatInLam" []

-- -- | Re-order a sequence of nested non-recursive let bindings.
-- -- The argument list should contain the let-bound variables, in the desired order.
-- reorderLets :: [String] -> Rewrite LCore

-- | Combine nested non-recursive lets into case of a tuple.
-- E.g. let {v1 = e1 ; v2 = e2 ; v3 = e3} in body ==> case (e1,e2,e3) of {(v1,v2,v3) -> body}
letTuple :: String -> Rewrite LCore
letTuple str = Transform $ method "letTuple" [toJSON str]

-- | Remove unused top-level binding(s).
-- prog-bind-nonrec-elim <+ prog-bind-rec-elim
progBindElim :: Rewrite LCore
progBindElim = Transform $ method "progBindElim" []

-- | Remove unused top-level binding(s).
-- v = e : prog ==> prog, if v is not free in prog and not exported.
progBindNonrecElim :: Rewrite LCore
progBindNonrecElim = Transform $ method "progBindNonrecElim" []

-- | Remove unused top-level binding(s).
-- v+ = e+ : prog ==> v* = e* : prog, where v* is a subset of v+ consisting
-- of vs that are free in prog or e+, or exported.
progBindRecElim :: Rewrite LCore
progBindRecElim = Transform $ method "progBindRecElim" []

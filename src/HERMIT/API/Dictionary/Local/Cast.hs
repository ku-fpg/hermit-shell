{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Local.Cast where

import HERMIT.API.Types

-- | castElimRefl <+ castElimSym
castElim :: RewriteH LCore
castElim = Transform $ method "castElim" []

-- | cast e co ==> e ; if co is a reflexive coercion
castElimRefl :: RewriteH LCore
castElimRefl = Transform $ method "castElimRefl" []

-- | removes pairs of symmetric casts
castElimSym :: RewriteH LCore
castElimSym = Transform $ method "castElimSym" []

-- | removes pairs of symmetric casts possibly separated by let or case forms
castElimSymPlus :: RewriteH LCore
castElimSymPlus = Transform $ method "castElimSymPlus" []

-- | (cast e (c1 -> c2)) x ==> cast (e (cast x (sym c1))) c2
castFloatApp :: RewriteH LCore
castFloatApp = Transform $ method "castFloatApp" []

-- | \\ x::a -> cast x (a -> b) ==> cast (\\x::a -> x) ((a -> a) -> (a -> b))
castFloatLam :: RewriteH LCore
castFloatLam = Transform $ method "castFloatLam" []

-- | removes casts regardless of whether it is safe to do so
castElimUnsafe :: RewriteH LCore
castElimUnsafe = Transform $ method "castElimUnsafe" []

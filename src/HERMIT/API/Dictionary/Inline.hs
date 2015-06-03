{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Inline where

import HERMIT.API.Types

-- -- | (Var v) ==> <defn of v>
-- inline :: Rewrite LCore

-- -- | Given a specific v, (Var v) ==> <defn of v>
-- inline :: Name -> Rewrite LCore

-- -- | If the current variable matches any of the given names, then inline it.
-- inline :: [String] -> Rewrite LCore

-- | if v is a case binder, replace (Var v) with the bound case scrutinee.
inlineCaseScrutinee :: Rewrite LCore
inlineCaseScrutinee = Transform $ method "inlineCaseScrutinee" []

-- | if v is a case binder, replace (Var v) with the bound case-alternative pattern.
inlineCaseAlternative :: Rewrite LCore
inlineCaseAlternative = Transform $ method "inlineCaseAlternative" []

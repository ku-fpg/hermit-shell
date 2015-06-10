{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module HERMIT.API.Dictionary.Unfold where

import HERMIT.API.Types

-- | Perform one or more beta-reductions.
betaReducePlus :: Rewrite LCore
betaReducePlus = Transform $ method "betaReducePlus" []

-- | @unfold :: Rewrite LCore@
-- In application f x y z, unfold f.
--
-- @unfold :: Name -> Rewrite LCore@
-- Inline a definition, and apply the arguments; traditional unfold.
--
-- @unfold :: [Name] -> Rewrite LCore@
-- Unfold a definition if it is named in the list.
unfold :: (ReturnType a ~ Rewrite LCore, RewriteWithOneOrMoreNames a)
       => a
unfold = rewriteWithOneOrMoreNames "unfold"

-- | Unfold a definition only if the function is fully applied.
unfoldSaturated :: Rewrite LCore
unfoldSaturated = Transform $ method "unfoldSaturated" []

-- | Specialize an application to its type and coercion arguments.
specialize :: Rewrite LCore
specialize = Transform $ method "specialize" []


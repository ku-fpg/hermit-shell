{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Unfold where

import HERMIT.API.Types

-- | Perform one or more beta-reductions.
betaReducePlus :: Rewrite LCore
betaReducePlus = Transform $ method "betaReducePlus" []

-- unfold :: Rewrite LCore
-- unfold :: OccurenceName -> Rewrite LCore
-- unfold :: [OccurenceName] -> Rewrite LCore

-- | Unfold a definition only if the function is fully applied.
unfoldSaturated :: Rewrite LCore
unfoldSaturated = Transform $ method "unfoldSaturated" []

-- | Specialize an application to its type and coercion arguments.
specialize :: Rewrite LCore
specialize = Transform $ method "specialize" []


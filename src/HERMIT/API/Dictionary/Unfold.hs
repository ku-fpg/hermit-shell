{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module HERMIT.API.Dictionary.Unfold where

import Data.Aeson

import HERMIT.API.Types

-- | Perform one or more beta-reductions.
betaReducePlus :: Rewrite LCore
betaReducePlus = Transform $ method "betaReducePlus" []

-- | In application f x y z, unfold f.
unfold :: Rewrite LCore
unfold = Transform $ method "unfold" []

-- | Inline a definition, and apply the arguments; traditional unfold.
unfoldWith :: String -> Rewrite LCore
unfoldWith str = Transform $ method "unfoldWith" [toJSON str]

-- | Unfold a definition if it is named in the list.
unfoldAny :: [String] -> Rewrite LCore
unfoldAny strs = Transform $ method "unfoldAny" [toJSON strs]

-- | Unfold a definition only if the function is fully applied.
unfoldSaturated :: Rewrite LCore
unfoldSaturated = Transform $ method "unfoldSaturated" []

-- | Specialize an application to its type and coercion arguments.
specialize :: Rewrite LCore
specialize = Transform $ method "specialize" []


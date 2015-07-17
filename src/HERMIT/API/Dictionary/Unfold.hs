{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module HERMIT.API.Dictionary.Unfold (
      betaReducePlus
    , unfold
    , unfoldWith
    , UnfoldArgs
    , unfoldSaturated
    , specialize
    ) where

import Data.Aeson

import HERMIT.API.Types

-- | Perform one or more beta-reductions.
betaReducePlus :: Rewrite LCore
betaReducePlus = Transform $ method "betaReducePlus" []

-- | In application f x y z, unfold f.
unfold :: Rewrite LCore
unfold = Transform $ method "unfold" []

-- |
-- unfoldWith :: OccurrenceName -> Rewrite LCore
--   Inline a definition, and apply the arguments; traditional unfold.
-- unfoldWith :: [OccurrenceName] -> Rewrite LCore
--   Unfold a definition if it is named in the list.
unfoldWith :: UnfoldArgs a => a -> Rewrite LCore
unfoldWith = Transform . unfoldMethod

-- | Class of types that can be used as an argument to 'unfoldWith'.
class UnfoldArgs a where
    unfoldMethod :: a -> Value

instance UnfoldArgs OccurrenceName where
    unfoldMethod str = method "unfoldWith" [toJSON str]

instance UnfoldArgs [OccurrenceName] where
    unfoldMethod strs = method "unfoldAny" [toJSON strs]

-- | Unfold a definition only if the function is fully applied.
unfoldSaturated :: Rewrite LCore
unfoldSaturated = Transform $ method "unfoldSaturated" []

-- | Specialize an application to its type and coercion arguments.
specialize :: Rewrite LCore
specialize = Transform $ method "specialize" []


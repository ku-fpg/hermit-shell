{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Remembered where

import Data.Aeson

import HERMIT.API.Types

-- | Remember the current binding, allowing it to be folded/unfolded in the future.
remember :: Name -> Transform LCore ()
remember nm = Transform $ method "remember" [toJSON nm]

-- | Unfold a remembered definition.
unfoldRemembered :: Name -> Rewrite LCore
unfoldRemembered nm = Transform $ method "unfoldRemembered" [toJSON nm]

-- | Fold a remembered definition.
foldRemembered :: Name -> Rewrite LCore
foldRemembered nm = Transform $ method "foldRemembered" [toJSON nm]

-- | Attempt to fold any of the remembered definitions.
foldAnyRemembered :: Rewrite LCore
foldAnyRemembered = Transform $ method "foldAnyRemembered" []


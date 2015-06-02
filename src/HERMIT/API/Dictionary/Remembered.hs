{-# LANGUAGE OverloadedStrings, KindSignatures, GADTs #-}
module HERMIT.API.Dictionary.Remembered where
        
import Data.Aeson

import HERMIT.API.Types

-- | Remember the current binding, allowing it to be folded/unfolded in the future.
remember :: Name -> Transform LCore () 
remember nm = Transform $ method "remember" [toJSON nm]

unfoldRemembered :: Name -> Rewrite LCore
unfoldRemembered nm = Transform $ method "unfoldRemembered" [toJSON nm]




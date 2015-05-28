{-# LANGUAGE OverloadedStrings, KindSignatures, GADTs #-}
module HERMIT.API.Path where
        
import Data.Aeson

import HERMIT.API.Types

-- | Find the path to the RHS of the binding of the named variable.
rhsOf :: Name -> Transform LCoreTC LocalPath --  :: TransformH LCore LocalPathH ()
rhsOf nm = Transform $ method "rhsOf" [toJSON nm]



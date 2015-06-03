{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Navigation where

import Data.Aeson

import HERMIT.API.Types

-- | Find the path to the RHS of the binding of the named variable.
rhsOf :: Name -> Transform LCoreTC LocalPath 
rhsOf nm = Transform $ method "rhsOf" [toJSON nm]

-- | Find the path to the binding of the named variable.
bindingOf :: Name -> Transform LCoreTC LocalPath
bindingOf nm = Transform $ method "bindingOf" [toJSON nm]



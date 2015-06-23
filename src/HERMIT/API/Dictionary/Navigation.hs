{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Navigation where

import Data.Aeson

import HERMIT.API.Types

-- | Find the path to the RHS of the binding of the named variable.
rhsOf :: Name -> Transform LCoreTC LocalPath
rhsOf nm = Transform $ method "rhsOf" [toJSON nm]

-- | Find the path to the binding group of the named variable.
bindingGroupOf :: String -> Transform LCoreTC LocalPath
bindingGroupOf str = Transform $ method "bindingGroupOf" [toJSON str]

-- | Find the path to the binding of the named variable.
bindingOf :: Name -> Transform LCoreTC LocalPath
bindingOf nm = Transform $ method "bindingOf" [toJSON nm]

-- | Find the path to the first occurrence of the named variable.
occurrenceOf :: Name -> Transform LCoreTC LocalPath
occurrenceOf nm = Transform $ method "occurenceOf" [toJSON nm]

-- | Find the path to the first application of the named variable.
applicationOf :: Name -> Transform LCoreTC LocalPath
applicationOf nm = Transform $ method "applicationOf" [toJSON nm]

-- -- | consider <c> focuses on the first construct <c>.
consider :: Considerable -> Transform LCore LocalPath
consider c = Transform $ method "consider" [toJSON c]

-- | arg n focuses on the (n-1)th argument of a nested application.
arg :: Int -> Transform LCore LocalPath
arg n = Transform $ method "arg" [toJSON n]

-- | Descend into the body after a sequence of lambdas.
lamsBody :: Transform LCore LocalPath
lamsBody = Transform $ method "lamsBody" []

-- | Descend into the body after a sequence of let bindings.
letsBody :: Transform LCore LocalPath
letsBody = Transform $ method "letsBody" []

-- | Descend to the end of a program
progEnd :: Transform LCore LocalPath
progEnd = Transform $ method "progEnd" []

-- -- | Focus on the parent of another focal point.
-- parentOf :: Transform LCore LocalPath -> Transform LCore LocalPath
--
-- -- | Focus on the parent of another focal point.
-- parentOf :: Transform LCoreTC LocalPath -> Transform LCoreTC LocalPath

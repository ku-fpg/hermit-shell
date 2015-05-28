{-# LANGUAGE OverloadedStrings, KindSignatures, GADTs #-}
module HERMIT.API.Path where
        
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe

import HERMIT.API.Types

-- | Find the path to the RHS of the binding of the named variable.
rhsOf :: Name -> Shell LocalPath --  :: TransformH LCore LocalPathH ()
rhsOf nm = Shell $ method "rhsOf" [toJSON nm]

goto :: LocalPath -> Shell ()
goto path = Shell $ method "goto" [toJSON path]


--display = ShellEffect $ method "display" []
--  TransformLCorePathH    :: TransformH LCore LocalPathH         -> TypedEffectH ()

-- goto =<< rhsOf 'f



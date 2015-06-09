{-# LANGUAGE OverloadedStrings #-}
module HERMIT.API.Dictionary.Query where

import Data.Aeson
import HERMIT.API.Types

-- | Display information about the current node.
info :: Transform LCoreTC String
info = Transform $ method "info" []

-- | Compare the definitions of two in-scope identifiers for alpha equality.
compareBoundIds :: HermitName -> HermitName -> Transform LCoreTC ()
compareBoundIds n1 n2 = Transform . method "compareBoundIds" $ map toJSON [n1, n2]

-- | Compare the core fragments at the end of the given paths for alpha-equality.
compareCoreAt :: Transform LCoreTC LocalPath
              -> Transform LCoreTC LocalPath
              -> Transform LCoreTC ()
compareCoreAt t1 t2 = Transform . method "compareCoreAt" $ map toJSON [t1, t2]

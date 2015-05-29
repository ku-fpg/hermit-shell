{-# LANGUAGE OverloadedStrings, KindSignatures, GADTs #-}
module HERMIT.API.KURE where
        
-- import Data.Aeson

import HERMIT.API.Types

-- | any-call (.. unfold command ..) applies an unfold command to all applications.
--   Preference is given to applications with more arguments.
anyCall :: Rewrite LCore -> Rewrite LCore
anyCall (Transform rr) = Transform $ method "anyCall" [rr]



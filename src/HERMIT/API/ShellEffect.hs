{-# LANGUAGE OverloadedStrings, KindSignatures, GADTs #-}
module HERMIT.API.ShellEffect where
        
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe

import HERMIT.API.Types

-- | redisplays current state.
display :: Shell ()
display = Shell $ method "display" []


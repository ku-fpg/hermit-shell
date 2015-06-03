{-# OPTIONS_GHC -fno-warn-orphans #-}
module HERMIT.Server.Parser.String where

import           Data.Aeson
import qualified Data.Text as Text

import           HERMIT.External (CoreString(..))
import           HERMIT.Server.Parser.Utils

instance External CoreString where
  parseExternal (String txt) = return . CoreString . Text.unpack $ txt
  parseExternal _            = fail "fail: RhsOfName"

{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Name where

import           Data.Aeson
import qualified Data.Text as Text

import           HERMIT.Name

import           HERMIT.Server.Parser.Utils

instance External RhsOfName where
  parseExternal (String txt) = return $ RhsOfName $ parseName $ Text.unpack $ txt
  parseExternal _            = fail "fail: RhsOfName"          

instance External BindingName where
  parseExternal (String txt) = return $ BindingName $ parseName $ Text.unpack $ txt
  parseExternal _            = fail "fail: BindingName"          


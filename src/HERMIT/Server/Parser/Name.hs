{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser.Name where

import           Data.Aeson
import qualified Data.Text as Text

import           HERMIT.Name
import           HERMIT.Lemma

import           HERMIT.Server.Parser.Utils

instance External HermitName where
  parseExternal (String txt) = return . parseName $ Text.unpack txt
  parseExternal _            = fail "fail: HermitName"

instance External RhsOfName where
  parseExternal (String txt) = return . RhsOfName . parseName $ Text.unpack txt
  parseExternal _            = fail "fail: RhsOfName"          

instance External BindingName where
  parseExternal (String txt) = return . BindingName . parseName $ Text.unpack txt
  parseExternal _            = fail "fail: BindingName"          

instance External LemmaName where
  parseExternal (String txt) = return . LemmaName $ Text.unpack txt
  parseExternal _            = fail "fail: LemmaName"

instance External OccurrenceName where
  parseExternal (String txt) = return . OccurrenceName . parseName $ Text.unpack txt
  parseExternal _            = fail "fail: OccurrenceName"

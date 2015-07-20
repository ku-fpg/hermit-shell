{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- This types that are common to both the Server and GHCI.
-- AJG: I think this should move to HERMIT.API.Types
module HERMIT.RemoteShell.Types where

import           Data.Aeson
import           Data.Default.Class

import           HERMIT.API.Types
import           HERMIT.Plugin.Renderer
import           HERMIT.PrettyPrinter.Common
import           HERMIT.RemoteShell.Orphanage ()

import           GHC.Generics

import           Prelude.Compat

import           System.IO (stdout)

-- 'Document' is the client facing, pre-rendered, pretty output.
newtype Document = Document DocH
  deriving (Generic)

instance ToJSON Document where
  toJSON (Document doc) = toJSON doc

instance FromJSON Document where
  parseJSON doc = Document <$> parseJSON doc

instance Response Document where
  printResponse (Document doc) = do
          print "Start Document"
          case lookup "unicode-terminal" shellRenderers of
            Nothing -> print "No unicode-terminal"
            Just f -> f stdout def (Right doc)
          print "<<DOCUMENT>>"
          print "End Document"

-- Extract the underlying DocH.
toDocH :: Document -> DocH
toDocH (Document d) = d


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- This types that are common to both the Server and GHCI.
-- AJG: I think this should move to HERMIT.API.Types
module HERMIT.RemoteShell.Types where

import Control.Applicative

import HERMIT.PrettyPrinter.Common

import HERMIT.GHCI.JSON

import Data.Aeson
import Data.Aeson.Types
import Data.Coerce

import GHC.Generics

import Text.PrettyPrint.MarkedHughesPJ as PP
import qualified Language.KURE.Path as KURE
import HERMIT.RemoteShell.Orphanage
import HERMIT.API.Types

import HERMIT.Plugin.Renderer

import Data.Default.Class

import System.IO (stdout)
        
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


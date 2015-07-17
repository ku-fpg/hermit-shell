{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- This types that are common to both the Server and GHCI.
module HERMIT.RemoteShell.Types where

import HERMIT.PrettyPrinter.Common

import HERMIT.GHCI.JSON
import           HERMIT.GHCI.Glyph

import Data.Aeson
import Data.Aeson.Types
import Data.Coerce

import GHC.Generics

import Text.PrettyPrint.MarkedHughesPJ as PP
import qualified Language.KURE.Path as KURE
import HERMIT.RemoteShell.Orphanage
        
-- 'Document' is the client facing, pre-rendered, pretty output.
newtype Document = Document DocH
  deriving (Generic)

instance ToJSON Document where
  toJSON (Document doc) = toJSON doc

-- Extract the underlying DocH.        
toDocH :: Document -> DocH
toDocH (Document d) = d


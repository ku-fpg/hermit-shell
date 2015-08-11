{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This is where *all* the orphan instances live.
-- All orphans are from outside the hermit-shell package.
-- Thus, this module should not depend on anything from the
-- hermit-shell modules.

module HERMIT.RemoteShell.Orphanage where

import           Data.Aeson

import           HERMIT.Core (Crumb(..))
import           HERMIT.Lemma (Used(..))
import           HERMIT.Dictionary.Navigation (Considerable(..))
import           HERMIT.PrettyPrinter.Common (HermitMark(..), Attr(..)
                                             ,SyntaxForColor(..), ShowOption(..)
                                             ,PrettyOptions(..)
                                             ,PrettyPrinter(..))
import qualified HERMIT.PrettyPrinter.AST as AST
import qualified HERMIT.PrettyPrinter.Clean as Clean
import qualified HERMIT.PrettyPrinter.GHC as GHC

import qualified Language.KURE.Path as KURE

import           Text.PrettyPrint.MarkedHughesPJ as PP

-- Where possible, we use the GHC Generic overloading.

-- From package marked-pretty
instance ToJSON mark => ToJSON (MDoc mark)
instance FromJSON mark => FromJSON (MDoc mark)

-- From package hermit
instance ToJSON mark => ToJSON (TextDetails mark)
instance FromJSON mark => FromJSON (TextDetails mark)

instance ToJSON HermitMark
instance FromJSON HermitMark
instance ToJSON Attr
instance FromJSON Attr
instance ToJSON SyntaxForColor
instance FromJSON SyntaxForColor

instance ToJSON Crumb
instance FromJSON Crumb

instance ToJSON Used
instance FromJSON Used

instance ToJSON Considerable
instance FromJSON Considerable

instance ToJSON ShowOption
instance FromJSON ShowOption
instance ToJSON PrettyOptions
instance FromJSON PrettyOptions

instance ToJSON PrettyPrinter where
    toJSON (PP _ _ opts tag) = object ["opts" .= opts, "tag" .= tag]

instance FromJSON PrettyPrinter where
    parseJSON (Object v) =
      do opts <- v .: "opts"
         tag <- v .: "tag"
         case tag of
           "ast"   -> return $! PP AST.ppForallQuantification AST.ppCoreTC 
                                   opts tag
           "clean" -> return $! PP Clean.ppForallQuantification Clean.ppCoreTC 
                                   opts tag
           "ghc"   -> return $! PP GHC.ppForallQuantification GHC.ppCoreTC 
                                   opts tag
           _       -> fail "parseJSON:  Unrecognized PrettyPrinter tag."
    parseJSON _ =     fail "parseJSON:  Cannot parse PrettyPrinter object."

-- From package kure
instance ToJSON a => ToJSON (KURE.SnocPath a) where
  toJSON (KURE.SnocPath p) = toJSON p
instance FromJSON a => FromJSON (KURE.SnocPath a) where
  parseJSON a = KURE.SnocPath <$> parseJSON a

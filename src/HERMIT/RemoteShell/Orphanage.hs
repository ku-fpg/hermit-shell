{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This is where *all* the orphan instances live.
-- All orphans are from outside the hermit-shell package.
-- Thus, this module should not depend on anything from the
-- hermit-shell modules.

module HERMIT.RemoteShell.Orphanage where

import HERMIT.PrettyPrinter.Common

import           Control.Applicative
import           Control.Monad

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Coerce

import           GHC.Generics

import           HERMIT.Core (Crumb(..))

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

instance ToJSON Crumb where
    -- cases where there are fields
    toJSON (Rec_Def i)         = object [ "crumb" .= ("Rec_Def" :: String)         , "n" .= i ]
    toJSON (Case_Alt i)        = object [ "crumb" .= ("Case_Alt" :: String)        , "n" .= i ]
    toJSON (Alt_Var i)         = object [ "crumb" .= ("Alt_Var" :: String)         , "n" .= i ]
    toJSON (TyConApp_Arg i)    = object [ "crumb" .= ("TyConApp_Arg" :: String)    , "n" .= i ]
    toJSON (TyConAppCo_Arg i)  = object [ "crumb" .= ("TyConAppCo_Arg" :: String)  , "n" .= i ]
    toJSON (AxiomInstCo_Arg i) = object [ "crumb" .= ("AxiomInstCo_Arg" :: String) , "n" .= i ]
    -- catch all for nullary constructors
    toJSON cr = object [ "crumb" .= show cr ]

instance FromJSON Crumb where
    parseJSON (Object v) = do
        cstr :: String <- v .: "crumb"
        case cstr of
            "Rec_Def"         -> Rec_Def         <$> v .: "n"
            "Case_Alt"        -> Case_Alt        <$> v .: "n"
            "Alt_Var"         -> Alt_Var         <$> v .: "n"
            "TyConApp_Arg"    -> TyConApp_Arg    <$> v .: "n"
            "TyConAppCo_Arg"  -> TyConAppCo_Arg  <$> v .: "n"
            "AxiomInstCo_Arg" -> AxiomInstCo_Arg <$> v .: "n"
            _ -> return $ read cstr
    parseJSON _          = mzero

-- From package kure
instance ToJSON a => ToJSON (KURE.SnocPath a) where
  toJSON (KURE.SnocPath p) = toJSON p
instance FromJSON a => FromJSON (KURE.SnocPath a) where
  parseJSON a = KURE.SnocPath <$> parseJSON a

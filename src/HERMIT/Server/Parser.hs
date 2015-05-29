{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
module HERMIT.Server.Parser where

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Applicative
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy as State

import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseMaybe, Parser)
import           Data.Char (isSpace)
import           Data.Either
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text

import           HERMIT.Dictionary
import           HERMIT.External hiding (External, external)
import           HERMIT.Kernel
import           HERMIT.Kure hiding ((<$>),(<*>))

import           HERMIT.Name (mkRhsOfPred, RhsOfName(..), parseName)

import           HERMIT.Plugin
import           HERMIT.Plugin.Builder
import           HERMIT.Plugin.Types

import           HERMIT.PrettyPrinter.Common (po_width, PrettyOptions, DocH)

import           HERMIT.Shell.Command
import           HERMIT.Shell.Completion
import           HERMIT.Shell.Externals
import           HERMIT.Shell.Types hiding (clm)

import           HERMIT.GHCI.JSON
import           HERMIT.GHCI.Renderer
import           HERMIT.GHCI.Types

import           HERMIT.Shell.ShellEffect

import           HERMIT.Context

import           System.IO (Handle)

import           HERMIT.Server.Parser.ShellEffect
import           HERMIT.Server.Parser.Transform
import           HERMIT.Server.Parser.Utils
import           Debug.Trace


-- NOTES
--  * exprToDyns has useful info about building types

parseCLT :: (MonadIO m, Functor m) => Aeson.Value -> Maybe (CLT m Aeson.Value)
parseCLT = parseMaybe parseTopLevel

parseTopLevel :: (MonadIO m, Functor m)
              => Aeson.Value -> Parser (CLT m Aeson.Value)
parseTopLevel v = fmap (const (toJSON ())) 
               <$> performTypedEffectH (show v) 
               <$> (parseExternal v :: Parser (TypedEffectH ()))

instance External (TypedEffectH ()) where
  parseExternal = alts 
    [ fmap ShellEffectH . parseExternal
    , external "setPath" (SetPathH :: TransformH LCoreTC LocalPathH -> TypedEffectH ())
    ]


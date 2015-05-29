{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeFamilies, DefaultSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser where

import           Control.Applicative
import           Control.Monad.Reader

import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseMaybe, Parser)
import           HERMIT.Kure hiding ((<$>),(<*>))

import           HERMIT.Shell.Command
import           HERMIT.Shell.Types hiding (clm)


import           HERMIT.Context

import           HERMIT.Server.Parser.ShellEffect()
import           HERMIT.Server.Parser.Transform()
import           HERMIT.Server.Parser.Utils

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
       ["sets the path"]
    ]


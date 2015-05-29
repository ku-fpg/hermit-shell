{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HERMIT.Server.Parser where

import           Control.Monad.Reader

import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseMaybe, Parser)

import           HERMIT.Context
import           HERMIT.Kure hiding ((<$>),(<*>))
import           HERMIT.Shell.Command
import           HERMIT.Shell.Types hiding (clm)

-- import           HERMIT.Context

import           HERMIT.Server.Parser.QueryFun()
import           HERMIT.Server.Parser.ShellEffect()
import           HERMIT.Server.Parser.Rewrite()
import           HERMIT.Server.Parser.Transform()
import           HERMIT.Server.Parser.Utils

import           Prelude.Compat

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
  parseExternals =
    [ fmap ShellEffectH . parseExternal
    , external "setPath" (SetPathH :: TransformH LCoreTC LocalPathH -> TypedEffectH ())
        ["sets the path"]
    , external "query"   (QueryH :: QueryFun -> TypedEffectH ())
        ["performs query"]
    , external "rewrite" (RewriteLCoreH :: RewriteH LCore -> TypedEffectH ())
       ["performs query"]
    , external "eval" (EvalH :: String -> TypedEffectH ())
       ["performs legacy shell"]
    ]

